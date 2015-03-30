{-|
Module      : Tree
Description : Tests for Network.Kademlia.Tree

Tests specific to Network.Kademlia.Tree.
-}

module Tree where

import Test.QuickCheck

import qualified Network.Kademlia.Tree as T
import Network.Kademlia.Types

import TestTypes

-- | Check wether an inserted Node is retrievable
insertCheck :: IdType -> Node IdType -> Bool
insertCheck id node = T.lookup tree (nodeId node) == Just node
    where tree = T.insert (T.create id) node

-- | Make sure a deleted Node can't be retrieved anymore
deleteCheck :: IdType -> Node IdType -> Bool
deleteCheck id node = T.lookup tree id == Nothing
    where tree = T.delete origin . nodeId $ node
          origin = T.insert (T.create id) node
