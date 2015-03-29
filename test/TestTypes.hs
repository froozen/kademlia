{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

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
import Data.Bits (testBit)

import Network.Kademlia.Types

newtype IdType = IT { getBS :: B.ByteString } deriving (Eq, Show)

-- A simple 32-byte ByteString
instance Serialize IdType where
    toBS = getBS
    fromBS bs = if B.length bs >= 32
        then Right $ first IT . B.splitAt 32 $ bs
        else Left "ByteString to short."

instance Serialize String where
    toBS = C.pack . show
    fromBS s =
        case (reads :: ReadS String) . C.unpack $ s of
            []               -> Left "Failed to parse string."
            (result, rest):_ -> Right (result, C.pack rest)

instance Arbitrary IdType where
    arbitrary = do
        str <- vectorOf 32 arbitrary
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
        , liftM2 STORE arbitrary arbitrary
        , liftM FIND_NODE arbitrary
        , liftM RETURN_NODES $ vectorOf 15 arbitrary
        , liftM FIND_VALUE arbitrary
        , liftM RETURN_VALUE arbitrary
        ]

instance (Arbitrary i, Arbitrary v) => Arbitrary (Signal i v) where
    arbitrary = liftM2 Signal arbitrary arbitrary

instance (Arbitrary i) => Arbitrary (Node i) where
    arbitrary = liftM2 Node arbitrary arbitrary
