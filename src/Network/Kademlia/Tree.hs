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

-- | Type used for building the Node Storage Tree
data NodeTree i =
      TreeNode (NodeTree i) Bool (KBucket i)
    | End

-- | Apply a function to the KBucket a Node with a given Id would be in
applyTo :: (Serialize i, Eq i) =>
           (KBucket i -> a) -- ^ Function to apply at matched position
        -> a                -- ^ Default value for End element
        -> NodeTree i       -- ^ NodeTree to apply to
        -> i                -- ^ Position to apply at
        -> a
applyTo f end tree id = go tree $ toByteStruct id
    where go End _ = end
          go (TreeNode next bit bucket) (b:bs)
            | b == bit  = go next bs
            | otherwise = f bucket

-- | Modify a NodeTree at the position a Node with a given Id would have
modifyTree :: (Serialize i, Eq i) =>
              (NodeTree i -> NodeTree i) -- ^ Function to apply to
                                         --   corresponding TreeNode
           -> NodeTree i                 -- ^ NodeTree to modify
           -> i                          -- ^ Position to modify at
           -> NodeTree i
modifyTree f tree id = go tree $ toByteStruct id
    where go End _ = End
          go this@(TreeNode next bit bucket) (b:bs)
            | b == bit  = TreeNode (go next bs) bit bucket
            | otherwise = f this

-- | Create a NodeTree corresponding to the Owner-Node's Id
create :: (Serialize i) => i -> NodeTree i
create id = foldr assemble End $ toByteStruct id
    where assemble bit tree = TreeNode tree bit []

-- | Insert a node into a NodeTree
insert :: (Serialize i, Eq i) => NodeTree i -> Node i -> NodeTree i
insert tree node = modifyTree f tree $ nodeId node
    where f this@(TreeNode next bit bucket)
            | node `notElem` bucket = TreeNode next bit (node:bucket)
            | otherwise = this

-- | Lookup a node within a NodeTree
lookup :: (Serialize i, Eq i) => NodeTree i -> i -> Maybe (Node i)
lookup tree id = applyTo f Nothing tree id
    where f = find $ idMatches id

-- | Delete a Node corresponding to a supplied Id from a NodeTree
delete :: (Serialize i, Eq i) => NodeTree i -> i -> NodeTree i
delete tree id = modifyTree f tree id
    where f (TreeNode next bit bucket) =
            TreeNode next bit $ filter (idMatches id) bucket

-- | Helper function used for KBucket manipulation
idMatches :: (Eq i) => i -> Node i -> Bool
idMatches id = (id ==) . nodeId
