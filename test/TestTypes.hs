{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances       #-}

{-|
Module      : TestTypes
Description : Types and Generators needed for general testing
-}

module TestTypes
       ( IdType    (..)
       , NodeBunch (..)
       , IdBunch   (..)
       ) where


import           Control.Arrow             (first)
import           Control.Monad             (liftM, liftM2)
import           Data.Binary               (Binary)
import qualified Data.ByteString           as B
import qualified Data.ByteString.Char8     as C
import           Data.Function             (on)
import           Data.List                 (nubBy)
import           Data.Word                 (Word16)
import           Network.Socket            (PortNumber)

import           Test.QuickCheck           (Arbitrary (..), Gen, oneof, suchThat,
                                            vectorOf)

import           Network.Kademlia.Instance (BanState (..))
import           Network.Kademlia.Types    (Command (..), Node (..), Peer (..),
                                            Serialize (..), Signal (..))

newtype IdType = IT
    { getBS :: B.ByteString
    } deriving (Eq, Ord, Binary)

-- Custom show instance
instance Show IdType where
    show = show . getBS

-- A simple 5-byte ByteString
instance Serialize IdType where
    toBS = getBS
    fromBS bs = if B.length bs >= 5
        then Right $ first IT . B.splitAt 5 $ bs
        else Left "ByteString to short."

instance Serialize String where
    toBS = C.pack . show
    fromBS s =
        case (reads :: ReadS String) . C.unpack $ s of
            []               -> Left "Failed to parse string."
            (result, rest):_ -> Right (result, C.pack rest)

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
        , liftM2 (RETURN_NODES 0) arbitrary $ vectorOf 30 arbitrary
        , liftM FIND_VALUE arbitrary
        , liftM2 RETURN_VALUE arbitrary arbitrary
        ]

instance (Arbitrary i, Arbitrary v) => Arbitrary (Signal i v) where
    arbitrary = liftM2 Signal arbitrary arbitrary

instance (Arbitrary i) => Arbitrary (Node i) where
    arbitrary = liftM2 Node arbitrary arbitrary

-- | This enables me to specifiy a new Arbitrary instance
newtype NodeBunch i = NB {
      nodes :: [Node i]
    } deriving (Show)

-- | Make sure all Ids are unique
instance (Arbitrary i, Eq i) => Arbitrary (NodeBunch i) where
    arbitrary = liftM NB $ vectorOf 20 arbitrary `suchThat` individualIds
        where individualIds = individual ((==) `on` nodeId)

individual :: (a -> a -> Bool) -> [a] -> Bool
individual eq s = length s == (length . clear $ s)
    where clear = nubBy eq

-- | This is needed for the Implementation tests
newtype IdBunch i = IB {
      getIds :: [i]
    } deriving (Show)

instance (Arbitrary i, Eq i) => Arbitrary (IdBunch i) where
    arbitrary = liftM IB $ vectorOf 20 arbitrary `suchThat` individual (==)

instance Arbitrary BanState where
    arbitrary = oneof
        [ return BanForever
        , return NoBan
        , BanTill <$> arbitrary
        ]
