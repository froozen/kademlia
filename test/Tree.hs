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


import           Control.Monad           (join, filterM)
import           Data.Foldable           (foldrM)
import           Data.Function           (on)
import           Data.List               (sort, sortBy)
import           Data.Maybe              (isJust)
import qualified Data.Set                as S
import           System.Random           (mkStdGen)
import           Test.QuickCheck         (Property, conjoin, counterexample, property)

import           Network.Kademlia.Config (WithConfig, defaultConfig, usingConfig, k)
import qualified Network.Kademlia.Tree   as T
import           Network.Kademlia.Types  (Node (..), Serialize (..), distance)

import           TestTypes               (IdType (..), NodeBunch (..))

usingDefaultConfig :: WithConfig a -> a
usingDefaultConfig = flip usingConfig defaultConfig

-- | Helper method for lookup checking
lookupCheck :: (Serialize i, Eq i) => T.NodeTree i -> Node i -> Bool
lookupCheck tree node = usingDefaultConfig (T.lookup tree (nodeId node)) == Just node

-- | Check wether an inserted Node is retrievable
insertCheck :: IdType -> Node IdType -> Bool
insertCheck nid node = usingDefaultConfig $ do
    tree <- join $ T.insert <$> T.create nid <*> pure node
    return $ lookupCheck tree node

-- | Make sure a deleted Node can't be retrieved anymore
deleteCheck :: IdType -> Node IdType -> Bool
deleteCheck nid node = usingDefaultConfig $ do
    origin <- join $ T.insert <$> T.create nid <*> pure node
    tree <- T.delete origin . nodeId $ node
    return . not . lookupCheck tree $ node

withTree :: (T.NodeTree IdType -> [Node IdType] -> WithConfig a) ->
            NodeBunch IdType -> IdType -> a
withTree f bunch nid = usingDefaultConfig $ do
    tree <- join $ foldrM (flip T.insert) <$> (T.create nid) <*> pure (nodes bunch)
    f tree $ nodes bunch

splitCheck :: NodeBunch IdType -> IdType -> Property
splitCheck = withTree $ \tree nodes ->
    return . conjoin . foldr (foldingFunc tree) [] $ nodes
  where
          tree `contains` node = node `elem` T.toList tree

          foldingFunc tree node props = prop : props
            where prop =
                    counterexample ("Failed to find " ++ show node) $
                  -- There is the possibiliy that nodes weren't inserted
                  -- because of full buckets.
                    lookupCheck tree node || not (tree `contains` node)

-- | Make sure the bucket sizes end up correct
bucketSizeCheck :: NodeBunch IdType -> IdType -> Bool
bucketSizeCheck = withTree $ \tree _ -> return $ T.fold foldingFunc True tree
    where foldingFunc _ False = False
          foldingFunc b _     = length b <= k defaultConfig

-- | Make sure refreshed Nodes are actually refreshed
refreshCheck :: NodeBunch IdType -> IdType -> Bool
refreshCheck = withTree $ \tree nodes -> do
    let node = last nodes
        foldingFunc _  False = False
        foldingFunc b _      = node `notElem` b
                               || head b == node
    refreshed <- T.insert tree node
    return $ T.fold foldingFunc True refreshed

-- | Make sure findClosest returns the Node with the closest Ids of all nodes
--   in the tree.
findClosestCheck :: IdType -> NodeBunch IdType -> IdType -> Property
findClosestCheck nid = withTree $ \tree nodes -> do
    let contains node = isJust <$> T.lookup tree (nodeId node)
        distanceF = distance nid . nodeId
    contained <- filterM contains nodes
    treeClosest <- T.findClosest tree nid $ k defaultConfig
    packed <- zip contained <$> mapM distanceF contained

    let g node props = counterexample (text node) (prop node):props
                         where prop node' = node' `elem` treeClosest
                               text node' = "Failed to find: " ++ show node'

        manualClosest = map fst . take (k defaultConfig) . sort' $ packed
        sort' = sortBy $ \(_, a) (_, b) -> compare a b

    return . conjoin . foldr g [] $ manualClosest

-- | Check that 'T.pickupNotClosest' doesn't return closest nodes.
pickupNotClosestDifferentCheck :: IdType -> NodeBunch IdType -> IdType -> Property
pickupNotClosestDifferentCheck nid = withTree $ \tree _ -> do
    closest    <- T.findClosest tree nid $ k defaultConfig
    let notClosest = T.pickupRandom tree (k defaultConfig) closest (mkStdGen 42)
    return . property $ all (`notElem` notClosest) closest

-- | Make sure `toView` represents tree correctly
viewCheck :: NodeBunch IdType -> IdType -> Bool
viewCheck = withTree $ \tree nodes -> do
    originId  <- T.extractId tree
    let view = T.toView tree
    sorted <- mapM (\bucket -> sort <$> mapM (distance originId . nodeId) bucket) view
              -- distance to this node increases from bucket to bucket
    return $  increases sorted
              -- and view contains all nodes from tree
           && sameElements nodes (concat view)
  where
    increases x  = x == sort x
    sameElements = (==) `on` S.fromList
