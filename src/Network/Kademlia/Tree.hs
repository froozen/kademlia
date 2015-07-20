{-|
Module      : Network.Kademlia.Tree
Description : Implementation of the Node Storage Tree

Network.Kademlia.Tree implements the Node Storage Tree used to store
and look up the known nodes.

This module is designed to be used as a qualified import.
-}

module Network.Kademlia.Tree
    ( NodeTree
    , create
    , insert
    , lookup
    , delete
    , handleTimeout
    , findClosest
    , extractId
    , toList
    , fold
    ) where

import Prelude hiding (lookup)
import Network.Kademlia.Types
import qualified Data.List as L (find, delete)

data NodeTree i = NodeTree ByteStruct (NodeTreeElem i)

data NodeTreeElem i = Split (NodeTreeElem i) (NodeTreeElem i)
                    | Bucket [(Node i, Int)]

type NodeTreeFunction i a = Int -> Bool -> [(Node i, Int)] -> a

-- | Modify the position in the tree where the supplied id would be
modifyAt :: (Serialize i) =>
            NodeTree i -> i -> NodeTreeFunction i (NodeTreeElem i)
         -> NodeTree i
modifyAt (NodeTree idStruct elem) id f =
    let targetStruct = toByteStruct id
        newElems     = go idStruct targetStruct 0 True elem
    in  NodeTree idStruct newElems
    where -- This function is partial, but we know that there will alwasys be a
          -- bucket at the end. Therefore, we don't have to check for empty
          -- ByteStructs
          --
          -- Apply the function to the position of the bucket
          go _ _ depth valid (Bucket b) = f depth valid b
          -- If the bit is a 0, go left
          go (i:is) (False:ts) depth valid (Split left right) =
               let new = go is ts (depth + 1) (valid && not i) left
               in  Split new right
          -- Otherwise, continue to the right
          go (i:is) (True:ts) depth valid (Split left right) =
               let new = go is ts (depth + 1) (valid && i) right
               in  Split left new

-- | Modify and apply a function at the position in the tree where the
--   supplied id would be
bothAt :: (Serialize i) =>
            NodeTree i -> i -> NodeTreeFunction i (NodeTreeElem i, a)
         -> (NodeTree i, a)
bothAt (NodeTree idStruct elem) id f =
    let targetStruct    = toByteStruct id
        (newElems, val) = go idStruct targetStruct 0 True elem
    in  (NodeTree idStruct newElems, val)
    where -- This function is partial, but we know that there will alwasys be a
          -- bucket at the end. Therefore, we don't have to check for empty
          -- ByteStructs
          --
          -- Apply the function to the position of the bucket
          go _ _ depth valid (Bucket b) = f depth valid b
          -- If the bit is a 0, go left
          go (i:is) (False:ts) depth valid (Split left right) =
               let (new, val) = go is ts (depth + 1) (valid && not i) left
               in  (Split new right, val)
          -- Otherwise, continue to the right
          go (i:is) (True:ts) depth valid (Split left right) =
               let (new, val) = go is ts (depth + 1) (valid && i) right
               in  (Split left new, val)

-- | Apply a function to the bucket the supplied id would be located in
applyAt :: (Serialize i) => NodeTree i -> i -> NodeTreeFunction i a -> a
applyAt (NodeTree idStruct elem) id f =
    let targetStruct = toByteStruct id
    in go idStruct targetStruct 0 True elem
    where -- This function is partial for the same reason as in modifyAt
          --
          -- Apply the function
          go _ _ depth valid (Bucket b) = f depth valid b
          -- If the bit is a 0, go left
          go (i:is) (False:ts) depth valid (Split left _) =
               go is ts (depth + 1) (valid && not i) left
          -- Otherwise, continue to the right
          go (i:is) (True:ts) depth valid (Split _ right) =
               go is ts (depth + 1) (valid && i) right

-- | Create a NodeTree corresponding to the id
create :: (Serialize i) => i -> NodeTree i
create id = NodeTree (toByteStruct id) . Bucket $ []

-- | Lookup a node within a NodeTree
lookup :: (Serialize i, Eq i) => NodeTree i -> i -> Maybe (Node i)
lookup tree id = applyAt tree id f
    where f _ _ = L.find (idMatches id) . map fst

-- | Delete a Node corresponding to a supplied Id from a NodeTree
delete :: (Serialize i, Eq i) => NodeTree i -> i -> NodeTree i
delete tree id = modifyAt tree id f
    where f _ _ = Bucket . filter (not . idMatches id . fst)

-- | Handle a timed out node by incrementing its timeoutCount and deleting it
--  if the count exceeds the limit. Also, return wether it's reasonable to ping
--  the node again.
handleTimeout :: (Serialize i, Eq i) => NodeTree i -> i -> (NodeTree i, Bool)
handleTimeout tree id = bothAt tree id f
    where f _ _ b = case L.find (idMatches id . fst) b of
            -- Delete a node that exceeded the limit. Don't contact it again
            --   as it is now considered dead
            Just x@(_, 4) -> (Bucket . L.delete x $ b, False)
            -- Increment the timeoutCount
            Just x@(n, timeoutCount) ->
                 (Bucket $ (n, timeoutCount + 1) : L.delete x b, True)
            -- Don't contact an unknown node a second time
            Nothing -> (Bucket b, False)

-- | Refresh the node corresponding to a supplied Id by placing it at the first
--   index of it's KBucket and reseting its timeoutCount, then return a Bucket
--   NodeTreeElem
refresh :: (Serialize i, Eq i) => Node i -> [(Node i, Int)] -> NodeTreeElem i
refresh node b = Bucket $ case L.find (idMatches (nodeId node) . fst) b of
                    Just x@(n, _) -> (n, 0) : L.delete x b
                    _         -> b

-- | Insert a node into a NodeTree
insert :: (Serialize i, Eq i) => NodeTree i -> Node i -> NodeTree i
insert tree node = if applyAt tree (nodeId node) needsSplit
                   -- Split the tree before inserting, when it makes sense
                   then let splitTree = split tree . nodeId $ node
                        in insert splitTree node
                   -- Insert the node
                   else modifyAt tree (nodeId node) doInsert

    where needsSplit depth valid b =
            let maxDepth = (length . toByteStruct . nodeId $ node) - 1
            in  -- A new node will be inserted
                node `notElem` map fst b &&
                -- The bucket is full
                length b >= 7 &&
                -- The bucket may be split
                (depth < 5 || valid) && depth <= maxDepth

          doInsert _ _ b
            -- Refresh an already existing node
            | node `elem` map fst b = refresh node b
            -- Simply insert the node, if the bucket isn't full
            | length b < 7 = Bucket $ (node, 0):b
            -- Don't insert the node otherwise
            | otherwise = Bucket b

-- | Split the KBucket the specified id would reside in into two and return a
--   Split NodeTreeElem
split :: (Serialize i) => NodeTree i -> i -> NodeTree i
split tree id = modifyAt tree id f
    where f depth _ b = let (left, right) = splitBucket depth b
                        in  Split (Bucket left) (Bucket right)

          -- Recursivly split the nodes into two buckets
          splitBucket _ []     = ([], [])
          splitBucket i (x@(n, _):ns) = let bs = toByteStruct . nodeId $ n
                                            bit = bs !! i
                                            (left, right) = splitBucket i ns
                                        in if bit
                                           then (left, x:right)
                                           else (x:left, right)

-- | Find the k closest Nodes to a given Id
findClosest :: (Serialize i) => NodeTree i -> i -> Int -> [Node i]
findClosest (NodeTree idStruct elem) id n =
    let targetStruct = toByteStruct id
    in go idStruct targetStruct elem n
    where -- This function is partial for the same reason as in modifyAt
          --
          -- Take the n closest nodes
          go _ _ (Bucket b) n
            | length b <= n = map fst b
            | otherwise = take n . sortByDistanceTo (map fst b) $ id
          -- Take the closest nodes from the left child first, if those aren't
          -- enough, take the rest from the right
          go (i:is) (False:ts) (Split left right) n =
            let result = go is ts left n
            in if length result == n
                then result
                else result ++ go is ts right n
          -- Take the closest nodes from the right child first, if those aren't
          -- enough, take the rest from the left
          go (i:is) (True:ts) (Split left right) n =
            let result = go is ts right n
            in if length result == n
                then result
                else result ++ go is ts left n

-- Extract original Id from NodeTree
extractId :: (Serialize i) => NodeTree i -> i
extractId (NodeTree id _) = fromByteStruct id

-- | Helper function used for KBucket manipulation
idMatches :: (Eq i) => i -> Node i -> Bool
idMatches id node = id == nodeId node

-- | Turn the NodeTree into a list of nodes
toList :: NodeTree i -> [Node i]
toList (NodeTree _ elems) = go elems
    where go (Split left right) = go left ++ go right
          go (Bucket b) = map fst b

-- | Fold over the buckets
fold :: ([Node i] -> a -> a) -> a -> NodeTree i -> a
fold f init (NodeTree _ elems) = go init elems
    where go a (Split left right) = let a' = go a left in go a' right
          go a (Bucket b) = f (map fst b) a
