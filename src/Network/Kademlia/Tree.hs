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
       , pickupRandom
       , findClosest
       , extractId
       , toView
       , toList
       , fold
       ) where

import           Prelude                 hiding (lookup)

import           Control.Monad.Random    (evalRand)
import           Data.Binary             (Binary)
import qualified Data.List               as L (delete, find, genericTake)
import           GHC.Generics            (Generic)
import           System.Random           (StdGen)
import           System.Random.Shuffle   (shuffleM)

import           Network.Kademlia.Config (WithConfig, getConfig, k, cacheSize, pingLimit)
import           Network.Kademlia.Types  (ByteStruct, Node (..), Serialize (..),
                                          fromByteStruct, sortByDistanceTo, toByteStruct)

data NodeTree i = NodeTree ByteStruct (NodeTreeElem i)
    deriving (Generic)

data NodeTreeElem i = Split (NodeTreeElem i) (NodeTreeElem i)
                    | Bucket ([(Node i, Int)], [Node i])
    deriving (Generic)

type NodeTreeFunction i a = Int -> Bool -> ([(Node i, Int)], [Node i]) -> WithConfig a

instance Binary i => Binary (NodeTree i)

instance Binary i => Binary (NodeTreeElem i)


-- | Modify the position in the tree where the supplied id would be
modifyAt :: (Serialize i) =>
            NodeTree i -> i -> NodeTreeFunction i (NodeTreeElem i)
         -> WithConfig (NodeTree i)
modifyAt (NodeTree idStruct treeElem) nid f = do
    targetStruct <- toByteStruct nid
    newElems <- go idStruct targetStruct 0 True treeElem
    return $ NodeTree idStruct newElems
  where -- This function is partial, but we know that there will alwasys be a
        -- bucket at the end. Therefore, we don't have to check for empty
        -- ByteStructs
        --
        -- Apply the function to the position of the bucket
        go _ _ depth valid (Bucket b) = f depth valid b
        -- If the bit is a 0, go left
        go (i:is) (False:ts) depth valid (Split left right) = do
             new <- go is ts (depth + 1) (valid && not i) left
             return $ Split new right
        -- Otherwise, continue to the right
        go (i:is) (True:ts) depth valid (Split left right) = do
             new <- go is ts (depth + 1) (valid && i) right
             return $ Split left new
        go _ _ _ _ _ = error "Fundamental error in @go@ function at 'modifyAt'"

-- | Modify and apply a function at the position in the tree where the
--   supplied id would be
bothAt :: (Serialize i) =>
            NodeTree i -> i -> NodeTreeFunction i (NodeTreeElem i, a)
         -> WithConfig (NodeTree i, a)
bothAt (NodeTree idStruct treeElem) nid f = do
    targetStruct <- toByteStruct nid
    (newElems, val) <- go idStruct targetStruct 0 True treeElem
    return (NodeTree idStruct newElems, val)
    where -- This function is partial, but we know that there will alwasys be a
          -- bucket at the end. Therefore, we don't have to check for empty
          -- ByteStructs
          --
          -- Apply the function to the position of the bucket
          go _ _ depth valid (Bucket b) = f depth valid b
          -- If the bit is a 0, go left
          go (i:is) (False:ts) depth valid (Split left right) = do
               (new, val) <- go is ts (depth + 1) (valid && not i) left
               return (Split new right, val)
          -- Otherwise, continue to the right
          go (i:is) (True:ts) depth valid (Split left right) = do
               (new, val) <- go is ts (depth + 1) (valid && i) right
               return (Split left new, val)
          go _ _ _ _ _ = error "Fundamental error in @go@ function in 'bothAt'"

-- | Apply a function to the bucket the supplied id would be located in
applyAt :: (Serialize i) => NodeTree i -> i -> NodeTreeFunction i a -> WithConfig a
applyAt (NodeTree idStruct treeElem) nid f = do
    targetStruct <- toByteStruct nid
    go idStruct targetStruct 0 True treeElem
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
          go _ _ _ _ _ = error "Fundamental error in @go@ function in 'applyAt'"

-- | Create a NodeTree corresponding to the id
create :: (Serialize i) => i -> WithConfig (NodeTree i)
create nid = NodeTree <$> (toByteStruct nid) <*> pure (Bucket ([], []))

-- | Lookup a node within a NodeTree
lookup :: (Serialize i, Eq i) => NodeTree i -> i -> WithConfig (Maybe (Node i))
lookup tree nid = applyAt tree nid f
    where f _ _ = return . L.find (idMatches nid) . map fst . fst

-- | Delete a Node corresponding to a supplied Id from a NodeTree
delete :: (Serialize i, Eq i) => NodeTree i -> i -> WithConfig (NodeTree i)
delete tree nid = modifyAt tree nid f
    where f _ _ (nodes, cache) =
              let deleted = filter (not . idMatches nid . fst) $ nodes
              in return $ Bucket (deleted, cache)

-- | Handle a timed out node by incrementing its timeoutCount and deleting it
--  if the count exceeds the limit. Also, return wether it's reasonable to ping
--  the node again.
handleTimeout :: (Serialize i, Eq i) => NodeTree i -> i -> WithConfig (NodeTree i, Bool)
handleTimeout tree nid = do
    pingLimit <- pingLimit <$> getConfig
    let f _ _ (nodes, cache) = return $ case L.find (idMatches nid . fst) nodes of
            -- Delete a node that exceeded the limit. Don't contact it again
            --   as it is now considered dead
            Just x@(_, bs) | bs == pingLimit -> (Bucket (L.delete x $ nodes, cache), False)
            -- Increment the timeoutCount
            Just x@(n, timeoutCount) ->
                 (Bucket ((n, timeoutCount + 1) : L.delete x nodes, cache), True)
            -- Don't contact an unknown node a second time
            Nothing -> (Bucket (nodes, cache), False)
    bothAt tree nid f

-- | Refresh the node corresponding to a supplied Id by placing it at the first
--   index of it's KBucket and reseting its timeoutCount, then return a Bucket
--   NodeTreeElem
refresh :: Eq i => Node i -> ([(Node i, Int)], [Node i]) -> NodeTreeElem i
refresh node (nodes, cache) =
         Bucket (case L.find (idMatches (nodeId node) . fst) nodes of
            Just x@(n, _) -> (n, 0) : L.delete x nodes
            _             -> nodes
            , cache)

-- | Insert a node into a NodeTree
insert :: (Serialize i, Eq i) => NodeTree i -> Node i -> WithConfig (NodeTree i)
insert tree node = do
    k <- k <$> getConfig
    cacheSize <- cacheSize <$> getConfig
    let needsSplit depth valid (nodes, _) = do
          maxDepth <- ((subtract 1) . length <$> toByteStruct (nodeId node))
          return $
            -- A new node will be inserted
            node `notElem` map fst nodes &&
            -- The bucket is full
            length nodes >= k &&
            -- The bucket may be split
            -- @georgeee: I peered at this code for ~30-40 mins.
            --   I clearly don't understand what was the reason to introduce `depth < 5`.
            --   Perhaps some kind of +\- 1, to not care about corner case
           (depth < 5 || valid) && depth <= maxDepth

        doInsert _ _ b@(nodes, cache)
          -- Refresh an already existing node
          | node `elem` map fst nodes = return $ refresh node b
          -- Simply insert the node, if the bucket isn't full
          | length nodes < k = return $ Bucket ((node, 0):nodes, cache)
          -- Move the node to the first spot, if it's already cached
          | node `elem` cache = return $ Bucket (nodes, node : L.delete node cache)
          -- Cache the node and drop older ones, if necessary
          | otherwise = return $ Bucket (nodes, node : take (cacheSize - 1) cache)
    r <- applyAt tree (nodeId node) needsSplit
    if r
    -- Split the tree before inserting, when it makes sense
    then let splitTree = split tree . nodeId $ node
         in flip insert node =<< splitTree
    -- Insert the node
    else modifyAt tree (nodeId node) doInsert

-- | Split the KBucket the specified id would reside in into two and return a
--   Split NodeTreeElem
split :: (Serialize i) => NodeTree i -> i -> WithConfig (NodeTree i)
split tree splitId = modifyAt tree splitId g
    where g depth _ (nodes, cache) = do
            (leftNodes, rightNodes) <- splitBucket depth fst nodes
            (leftCache, rightCache) <- splitBucket depth id cache
            return $ Split
                (Bucket (leftNodes, leftCache))
                (Bucket (rightNodes, rightCache))

          -- Recursivly split the nodes into two buckets
          splitBucket _ _ []     = return ([], [])
          splitBucket i f (n:ns) = do
              bs <- toByteStruct . nodeId . f $ n
              let bit = bs !! i
              (left, right) <- splitBucket i f ns
              return $ if bit
                       then (left, n:right)
                       else (n:left, right)

-- | Returns @n@ random nodes from @all \\ ignoredList@.
pickupRandom
    :: (Eq i)
    => NodeTree i
    -> Int
    -> [Node i]
    -> StdGen
    -> [Node i]
pickupRandom _ 0 _ _ = []
pickupRandom tree n ignoreList randGen =
    let treeList      = toList tree
        notIgnored     = filter (`notElem` ignoreList) treeList
        shuffledNodes = evalRand (shuffleM notIgnored) randGen
    in L.genericTake n shuffledNodes

-- | Find the k closest Nodes to a given Id
findClosest
    :: (Serialize i)
    => NodeTree i
    -> i
    -> Int
    -> WithConfig [Node i]
findClosest (NodeTree idStruct treeElem) nid n = do
    let
        chooseClosest nodes = take n <$> (sortByDistanceTo nodes $ nid)

        -- This function is partial for the same reason as in modifyAt
        --
        -- Take the n closest nodes
        go _ _ (Bucket (nodes, _))
          | length nodes <= n = return $ map fst nodes
          | otherwise         = chooseClosest $ map fst nodes
        -- Take the closest nodes from the left child first, if those aren't
        -- enough, take the rest from the right
        go (_:is) (False:ts) (Split left right) = do
          result <- go is ts left
          if length result == n
          then return result
          else (result ++) <$> go is ts right
        -- Take the closest nodes from the right child first, if those aren't
        -- enough, take the rest from the left
        go (_:is) (True:ts) (Split left right) = do
          result <- go is ts right
          if length result == n
          then return result
          else (result ++) <$> go is ts left
        go _ _ _ = error "Fundamental error in @go@ function in 'findClosest'"

    targetStruct <- toByteStruct nid
    chooseClosest =<< go idStruct targetStruct treeElem

-- Extract original Id from NodeTree
extractId :: (Serialize i) => NodeTree i -> WithConfig i
extractId (NodeTree nid _) = fromByteStruct nid

-- | Helper function used for KBucket manipulation
idMatches :: (Eq i) => i -> Node i -> Bool
idMatches nid node = nid == nodeId node

-- | Turn the NodeTree into a list of buckets, ordered by distance to origin node
toView :: NodeTree i -> [[Node i]]
toView (NodeTree bs treeElems) = go bs treeElems []
    where -- If the bit is 0, go left, then right
          go (False:is) (Split left right) = go is left . go is right
          -- Else go right first
          go (True:is)  (Split left right) = go is right . go is left
          go _          (Split _    _    ) = error "toView: unexpected Split"
          go _          (Bucket (b, _))    = (map fst b :)

-- | Turn the NodeTree into a list of nodes
toList :: NodeTree i -> [Node i]
toList = concat . toView

-- | Fold over the buckets
fold :: ([Node i] -> a -> a) -> a -> NodeTree i -> a
fold f start (NodeTree _ treeElems) = go start treeElems
    where go a (Split left right) = let a' = go a left in go a' right
          go a (Bucket b)         = f (map fst . fst $ b) a
