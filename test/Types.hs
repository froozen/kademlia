{-# LANGUAGE FlexibleInstances #-}

{-|
Module      : Types
Description : Types and Generators needed for general testing

-}

module Types where

import Test.QuickCheck

import Control.Monad (liftM, liftM2, liftM3)
import Control.Arrow (first)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Network.Socket (PortNumber)
import Data.Word(Word16)

import Network.Kademlia.Types

newtype IdType = IT { getBS :: B.ByteString } deriving (Eq, Show)

-- A simple 32-byte ByteString
instance Id IdType where
    toBS = getBS
    fromBS bs = if B.length bs >= 32
        then Right $ first IT . B.splitAt 32 $ bs
        else Left "ByteString to short."

instance Arbitrary IdType where
    arbitrary = do
        str <- vectorOf 32 arbitrary
        return $ IT $ C.pack str

instance Arbitrary PortNumber where
    arbitrary = liftM fromIntegral (arbitrary :: Gen Word16)

instance Arbitrary Peer where
    arbitrary = do
        host <- arbitrary `suchThat` \s -> ' ' `notElem` s && not (null s)
        port <- arbitrary
        return $ Peer host port

instance Arbitrary (Command IdType String) where
    arbitrary = oneof [
          return PING
        , liftM2 STORE arbitrary arbitrary
        , liftM FIND_NODE arbitrary
        , liftM RETURN_NODES $ vectorOf 15 arbitrary
        , liftM FIND_VALUE arbitrary
        , liftM RETURN_VALUE arbitrary
        ]

instance Arbitrary (Signal IdType String) where
    arbitrary = liftM3 Signal arbitrary arbitrary arbitrary

