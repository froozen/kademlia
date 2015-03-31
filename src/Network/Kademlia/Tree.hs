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
    ) where

import Network.Kademlia.Types
import Data.List (deleteBy, find, elemIndex)
import Prelude hiding (lookup, split)
import Control.Arrow (first, second)

-- | Type used for building the Node Storage Tree
type NodeTree i = [(Bool, Maybe (KBucket i))]

-- | Structure used for easier modification of the NodeTree
type Zipper i = (NodeTree i, NodeTree i)

-- | Move the Zipper along an Id
seek :: (Serialize i) => NodeTree i -> i -> Zipper i
seek tree id = go tree $ toByteStruct id
    where go [] _ = ([], [])
          go (pair@(bit, bucket):rest) (b:bs)
            | ends rest = ([], pair:rest)
            | bit == b  = first (pair:) $ go rest bs
            | otherwise = ([], pair:rest)

-- | Cheks wether a NodeTree ends
ends :: NodeTree i -> Bool
ends ((_, Just _):_)  = False
ends ((_, Nothing):_) = True

-- | Apply a function to the KBucket a Node with a given Id would be in
applyTo :: (Serialize i, Eq i) =>
           (KBucket i -> a) -- ^ Function to apply at matched position
        -> a                -- ^ Default value
        -> NodeTree i       -- ^ NodeTree to apply to
        -> i                -- ^ Position to apply at
        -> a
applyTo f end tree id = case seek tree id of
        (_, [])                 -> end
        (_, (_, Nothing):_)     -> end
        (_, (_, Just bucket):_) -> f bucket

-- | Modify a NodeTree at the position a Node with a given Id would have
modifyTreeAt :: (Serialize i, Eq i) =>
                ((Bool, Maybe (KBucket i)) -> (Bool, Maybe (KBucket i)))
                -- ^ Function to apply to corresponding TreeNode
             -> NodeTree i -- ^ NodeTree to modify
             -> i          -- ^ Position to modify at
             -> NodeTree i
modifyTreeAt f tree id = case seek tree id of
        (beg, [])       -> beg
        (beg, pair:end) -> beg ++ f pair : end

-- | Create a NodeTree corresponding to the Owner-Node's Id
create :: (Serialize i) => i -> NodeTree i
create id = zip (toByteStruct id) (repeat Nothing)

-- | Insert a node into a NodeTree
insert :: (Serialize i, Eq i, Ord i) => NodeTree i -> Node i -> NodeTree i
insert tree node = case seek tree . nodeId $ node of
        -- The tree is empty, create first KBucket
        (_, (b, Nothing):xs)       -> (b, Just [node]):xs

        -- Normal case
        (beg, (b, Just bucket):xs)
            -- The last bucket may always be split
            | full bucket && ends xs -> let new = split tree id
                                        in insert new node
            -- If the bucket is full and can't be split, the Node isn't inserted
            | full bucket -> tree
            -- Make sure all nodes are unique
            | node `notElem` bucket -> beg ++ (b, Just $ node:bucket):xs
            | otherwise -> tree

    where full b = length b >= 7

          -- Extract original Id from NodeTree
          bs = foldr (\x id -> fst x:id) [] tree
          id = let (Right (id, _)) = fromBS . fromByteStruct $ bs in id

-- | Split the last bucket
--
--   This function does some quite unsafe pattern matching for the sake of not
--   ending up even longer than it already is. It is only used internally and
--   all the assumptions made by those patterns are provable, so it's ok.
split :: (Serialize i, Ord i) => NodeTree i -> i -> NodeTree i
split tree id = let (begin, (b, Just bucket):xs) = seek tree id
                    (this, next) = doSplit bucket
                in begin ++ (b, Just this) : injectBucket next xs

    where doSplit []        = ([], [])
          doSplit (node:ns) =
            -- More matching bytes than the index means that a node can be
            -- moved to a later bucket.
            if countMatching (toByteStruct . nodeId $ node)
                             (toByteStruct id)               > index
              then second (node:) $ doSplit ns
              else first  (node:) $ doSplit ns

          index = let (beg, _) = seek tree id in length beg
          countMatching [] [] = 0
          countMatching (a:as) (b:bs)
            | a == b    = 1 + countMatching as bs
            | otherwise = 0

          injectBucket bucket ((b, _):xs) = (b, Just bucket):xs

-- | Lookup a node within a NodeTree
lookup :: (Serialize i, Eq i) => NodeTree i -> i -> Maybe (Node i)
lookup tree id = applyTo f Nothing tree id
    where f = find $ idMatches id

-- | Delete a Node corresponding to a supplied Id from a NodeTree
delete :: (Serialize i, Eq i) => NodeTree i -> i -> NodeTree i
delete tree id = modifyTreeAt f tree id
    where f = second . fmap $ filter g
          g node = not . idMatches id $ node

-- | Helper function used for KBucket manipulation
idMatches :: (Eq i) => i -> Node i -> Bool
idMatches id node = id == nodeId node
