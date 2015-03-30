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
import Data.List (deleteBy, find)
import Prelude hiding (lookup)
import Control.Arrow (first, second)

-- | Type used for building the Node Storage Tree
type NodeTree i = [(Bool, KBucket i)]

-- | Structure used for easier modification of the NodeTree
type Zipper i = (NodeTree i, NodeTree i)

-- | Move the Zipper along an Id
seek :: (Serialize i) => NodeTree i -> i -> Zipper i
seek tree id = go tree $ toByteStruct id
    where go [] _ = ([], [])
          go (pair@(bit, bucket):rest) (b:bs)
            | bit == b  = first (pair:) $ go rest bs
            | otherwise = ([], pair:rest)

-- | Apply a function to the KBucket a Node with a given Id would be in
applyTo :: (Serialize i, Eq i) =>
           (KBucket i -> a) -- ^ Function to apply at matched position
        -> a                -- ^ Default value for End element
        -> NodeTree i       -- ^ NodeTree to apply to
        -> i                -- ^ Position to apply at
        -> a
applyTo f end tree id = case seek tree id of
        (_, [])            -> end
        (_, (_, bucket):_) -> f bucket

-- | Modify a NodeTree at the position a Node with a given Id would have
modifyTreeAt :: (Serialize i, Eq i) =>
                ((Bool, KBucket i) -> (Bool, KBucket i))
                -- ^ Function to apply to corresponding TreeNode
             -> NodeTree i -- ^ NodeTree to modify
             -> i          -- ^ Position to modify at
             -> NodeTree i
modifyTreeAt f tree id = case seek tree id of
        (beg, [])       -> beg
        (beg, pair:end) -> beg ++ f pair : end

-- | Create a NodeTree corresponding to the Owner-Node's Id
create :: (Serialize i) => i -> NodeTree i
create id = zip (toByteStruct id) (repeat [])

-- | Insert a node into a NodeTree
insert :: (Serialize i, Eq i) => NodeTree i -> Node i -> NodeTree i
insert tree node = modifyTreeAt f tree $ nodeId node
    where f pair@(bit, bucket)
            | node `notElem` bucket = (bit, node:bucket)
            | otherwise = pair

-- | Lookup a node within a NodeTree
lookup :: (Serialize i, Eq i) => NodeTree i -> i -> Maybe (Node i)
lookup tree id = applyTo f Nothing tree id
    where f = find $ idMatches id

-- | Delete a Node corresponding to a supplied Id from a NodeTree
delete :: (Serialize i, Eq i) => NodeTree i -> i -> NodeTree i
delete tree id = modifyTreeAt f tree id
    where f = second $ filter (idMatches id)

-- | Helper function used for KBucket manipulation
idMatches :: (Eq i) => i -> Node i -> Bool
idMatches id = (id ==) . nodeId
