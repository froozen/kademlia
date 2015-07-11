{-|
Module      : Tree
Description : Tests for Network.Kademlia.Tree

Tests specific to Network.Kademlia.Tree.
-}

module Tree where

import Test.QuickCheck

import qualified Network.Kademlia.Tree as T
import Network.Kademlia.Types
import Control.Monad (liftM)
import Data.List (sortBy)
import Data.Maybe (isJust)

import TestTypes

-- | Helper method for lookup checking
lookupCheck :: (Serialize i, Eq i) => T.NodeTree i -> Node i -> Bool
lookupCheck tree node = T.lookup tree (nodeId node) == Just node

-- | Check wether an inserted Node is retrievable
insertCheck :: IdType -> Node IdType -> Bool
insertCheck id node = lookupCheck tree node
    where tree = T.insert (T.create id) node

-- | Make sure a deleted Node can't be retrieved anymore
deleteCheck :: IdType -> Node IdType -> Bool
deleteCheck id node = not . lookupCheck tree $ node
    where tree = T.delete origin . nodeId $ node
          origin = T.insert (T.create id) node

withTree :: (T.NodeTree IdType -> [Node IdType] -> a) ->
            NodeBunch IdType -> IdType -> a
withTree f bunch id = f tree $ nodes bunch
    where tree = foldr (flip T.insert) (T.create id) $ nodes bunch

splitCheck :: NodeBunch IdType -> IdType -> Property
splitCheck = withTree f
    where f tree nodes = conjoin . foldr (foldingFunc tree) [] $ nodes

          tree `contains` node = node `elem` T.toList tree

          foldingFunc tree node props = prop : props
            where prop =
                    counterexample ("Failed to find " ++ show node) $
                  -- There is the possibiliy that nodes weren't inserted
                  -- because of full buckets.
                    lookupCheck tree node || not (tree `contains` node)

-- | Make sure the bucket sizes end up correct
bucketSizeCheck :: NodeBunch IdType -> IdType -> Bool
bucketSizeCheck = withTree $ \tree _ -> T.fold foldingFunc True tree
    where foldingFunc _ False = False
          foldingFunc b  _    = length b <= 7

-- | Make sure refreshed Nodes are actually refreshed
refreshCheck :: NodeBunch IdType -> IdType -> Bool
refreshCheck = withTree f
    where f tree nodes = T.fold foldingFunc True refreshed
            where refreshed = T.insert tree node
                  node = last nodes
                  foldingFunc _  False = False
                  foldingFunc b _      = node `notElem` b
                                         || head b == node

-- | Make sure findClosest returns the Node with the closest Ids of all nodes
--   in the tree.
findClosestCheck :: IdType -> NodeBunch IdType -> IdType -> Property
findClosestCheck id = withTree f
    where f tree nodes = conjoin . foldr g [] $ manualClosest
           where g node props = counterexample (text node) (prop node):props
                  where prop node = node `elem` treeClosest
                        text node = "Failed to find: " ++ show node

                 treeClosest = T.findClosest tree id 7

                 contained = filter contains nodes
                 contains node = isJust . T.lookup tree . nodeId $ node

                 manualClosest = map fst . take 7 . sort $ packed
                 packed = zip contained $ map distanceF contained
                 distanceF = distance id . nodeId
                 sort = sortBy $ \(_, a) (_, b) -> compare a b
