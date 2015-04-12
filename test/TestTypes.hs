{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, DeriveGeneric #-}

{-|
Module      : TestTypes
Description : Types and Generators needed for general testing
-}

module TestTypes where

import Test.QuickCheck

import Control.Monad (liftM, liftM2, liftM3)
import Control.Arrow (first)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Network.Socket (PortNumber)
import Data.Word(Word16)
import Data.List (nubBy)
import GHC.Generics(Generic)
import Data.Binary

import Network.Kademlia.Types

newtype IdType = IT { getBS :: B.ByteString } deriving (Eq, Show, Ord, Generic)
instance Binary IdType

instance Arbitrary IdType where
    arbitrary = do
        str <- vectorOf 5 arbitrary
        return $ IT $ C.pack str

instance Arbitrary PortNumber where
    arbitrary = liftM fromIntegral (arbitrary :: Gen Word16)

instance Arbitrary Peer where
    arbitrary = do
        host <- arbitrary `suchThat` \s -> ' ' `notElem` s && not (null s)
                                           && length s < 20
        port <- arbitrary
        return $ Peer host port

instance (Arbitrary i, Arbitrary v) => Arbitrary (Command i v) where
    arbitrary = oneof [
          return PING
        , return PONG
        , liftM2 STORE arbitrary arbitrary
        , liftM FIND_NODE arbitrary
        , liftM2 RETURN_NODES arbitrary $ vectorOf 15 arbitrary
        , liftM FIND_VALUE arbitrary
        , liftM2 RETURN_VALUE arbitrary arbitrary
        ]

instance (Arbitrary i, Arbitrary v) => Arbitrary (Signal i v) where
    arbitrary = liftM2 Signal arbitrary arbitrary

instance (Arbitrary i) => Arbitrary (Node i) where
    arbitrary = liftM2 Node arbitrary arbitrary

-- | This enables me to specifiy a new Arbitrary instance
newtype NodeBunch i = NB {
      nodes :: KBucket i
    } deriving (Show)

-- | Make sure all Ids are unique
instance (Arbitrary i, Eq i) => Arbitrary (NodeBunch i) where
    arbitrary = liftM NB $ vectorOf 20 arbitrary `suchThat` individualIds
        where individualIds s = length s == (length . cleared $ s)
              cleared = nubBy (\a b -> nodeId a == nodeId b)

