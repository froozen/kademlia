{-|
Module      : Tree
Description : Tests for Network.Kademlia.Tree

Tests specific to Network.Kademlia.Tree.
-}

module Tree
       ( withTree
       , bucketSizeCheck
       , deleteCheck
       , findClosestCheck
       , insertCheck
       , lookupCheck
       , pickupNotClosestDifferentCheck
       , refreshCheck
       , splitCheck
       , viewCheck
       ) where


import           Data.Function           (on)
import           Data.List               (sort, sortBy)
import           Data.Maybe              (isJust)
import qualified Data.Set                as S
import           System.Random           (mkStdGen)
import           Test.QuickCheck         (Property, conjoin, counterexample, property)

import           Network.Kademlia.Config (k)
import qualified Network.Kademlia.Tree   as T
import           Network.Kademlia.Types  (Node (..), Serialize (..), distance)

import           TestTypes               (IdType (..), NodeBunch (..))

-- | Helper method for lookup checking
lookupCheck :: (Serialize i, Eq i) => T.NodeTree i -> Node i -> Bool
lookupCheck tree node = T.lookup tree (nodeId node) == Just node

-- | Check wether an inserted Node is retrievable
insertCheck :: IdType -> Node IdType -> Bool
insertCheck nid node = lookupCheck tree node
    where tree = T.insert (T.create nid) node

-- | Make sure a deleted Node can't be retrieved anymore
deleteCheck :: IdType -> Node IdType -> Bool
deleteCheck nid node = not . lookupCheck tree $ node
    where tree = T.delete origin . nodeId $ node
          origin = T.insert (T.create nid) node

withTree :: (T.NodeTree IdType -> [Node IdType] -> a) ->
            NodeBunch IdType -> IdType -> a
withTree f bunch nid = f tree $ nodes bunch
    where tree = foldr (flip T.insert) (T.create nid) $ nodes bunch

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
          foldingFunc b _     = length b <= k

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
findClosestCheck nid = withTree f
    where f tree nodes = conjoin . foldr g [] $ manualClosest
           where g node props = counterexample (text node) (prop node):props
                  where prop node' = node' `elem` treeClosest
                        text node' = "Failed to find: " ++ show node'

                 treeClosest = T.findClosest tree nid k

                 contained = filter contains nodes
                 contains node = isJust . T.lookup tree . nodeId $ node

                 manualClosest = map fst . take k . sort' $ packed
                 packed = zip contained $ map distanceF contained
                 distanceF = distance nid . nodeId
                 sort' = sortBy $ \(_, a) (_, b) -> compare a b

-- | Check that 'T.pickupNotClosest' doesn't return closest nodes.
pickupNotClosestDifferentCheck :: IdType -> NodeBunch IdType -> IdType -> Property
pickupNotClosestDifferentCheck nid = withTree verifyNotClosest
  where
    verifyNotClosest :: T.NodeTree IdType -> [Node IdType] -> Property
    verifyNotClosest tree _ =
        let closest    = T.findClosest tree nid k
            notClosest = T.pickupRandom tree k closest (mkStdGen 42)
        in property $ all (`notElem` notClosest) closest

-- | Make sure `toView` represents tree correctly
viewCheck :: NodeBunch IdType -> IdType -> Bool
viewCheck = withTree $
    \tree nodes ->
        let originId  = T.extractId tree
            view      = T.toView tree
            -- distance to this node increases from bucket to bucket
        in  increases [ dist | bucket <- view
                             , dist   <- sort $ map (distance originId . nodeId) bucket ]
            -- and view contains all nodes from tree
        &&  sameElements nodes (concat view)
  where
    increases x  = x == sort x
    sameElements = (==) `on` S.fromList
