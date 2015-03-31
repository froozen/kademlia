{-|
Module      : Tree
Description : Tests for Network.Kademlia.Tree

Tests specific to Network.Kademlia.Tree.
-}

module Tree where

import Test.QuickCheck
import Test.QuickCheck.Property as P

import qualified Network.Kademlia.Tree as T
import Network.Kademlia.Types
import Control.Monad (liftM)
import Data.List (nubBy, foldl')

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

-- | This enables me to specifiy a new Arbitrary instance
newtype NodeBunch i = NB {
      nodes :: KBucket i
    } deriving (Show)

-- | Make sure all Ids are unique
instance (Arbitrary i, Eq i) => Arbitrary (NodeBunch i) where
    arbitrary = liftM NB $ vectorOf 20 arbitrary `suchThat` individualIds
        where individualIds s = length s == (length . cleared $ s)
              cleared = nubBy (\a b -> nodeId a == nodeId b)

-- | Check wether Splitting works in accordance with lookup
splitCheck :: NodeBunch IdType -> IdType -> P.Result
splitCheck nb id = foldr foldingFunc result $ nodes nb
    where tree = foldr (flip T.insert) (T.create id) $ nodes nb
          result = P.result { ok = Just True }

          ((_, Nothing):_) `contains` node = False
          ((_, Just b):xs) `contains` node = node `elem` b
                                              || xs `contains` node

          foldingFunc node p
            | ok p /= Just True = p
            | otherwise = p {
                  P.ok = Just $ lookupCheck tree node
                  -- There is the possibiliy that nodes weren't inserted because
                  -- of full buckets.
                                || not (tree `contains` node)
                , P.reason = "Failed to find " ++ show node
            }

-- | Make sure the bucket sizes end up correct
bucketSizeCheck :: NodeBunch IdType -> IdType -> Bool
bucketSizeCheck nb id = foldr foldingFunc True tree
    where tree = foldr (flip T.insert) (T.create id) $ nodes nb

          foldingFunc _ False        = False
          foldingFunc (_, Nothing) _ = True
          foldingFunc (_, Just b)  _ = length b <= 7
