{-# LANGUAGE FlexibleInstances #-}

{-|
Module      : Protocol
Description : Test for Network.Kademlia.Protocol

Tests specific to Network.Kademlia.Protocol.
-}

module Protocol
    ( parseCheck
    ) where

import Test.QuickCheck

import Network.Kademlia.Types
import Network.Kademlia.Protocol
import Control.Arrow (first)
import Control.Monad (liftM, liftM2, liftM3)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Network.Socket (PortNumber)
import Data.Word(Word16)

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
        , liftM RETURN_NODES $ (listOf arbitrary) `suchThat` (not . null)
        , liftM FIND_VALUE arbitrary
        , liftM RETURN_VALUE arbitrary
        ]

instance Arbitrary (Signal IdType String) where
    arbitrary = liftM3 Signal arbitrary arbitrary arbitrary

-- | A signal is the same as its serialized form parsed
parseCheck :: Signal IdType String -> Bool
parseCheck s = test . parse (peer s) . serialize (peerId s) . command $ s
    where test (Left _) = False
          test (Right s') = s == s'

