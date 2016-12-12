{-|
Module      : Types
Description : Tests for Network.Kademlia.Types

Tests specific to Network.Kademlia.Types.
-}

module Types
       ( fromByteStructCheck
       , toByteStructCheck
       ) where

import           Data.Bits              (testBit)
import qualified Data.ByteString        as B
import           Test.QuickCheck        ()

import           Network.Kademlia.Types (fromByteStruct, toBS, toByteStruct)

import           TestTypes              (IdType)

-- | Checks wether toByteStruct converts corretctly
toByteStructCheck :: IdType -> Bool
toByteStructCheck nid = foldl foldingFunc True [0..length converted - 1]
    where converted = toByteStruct nid
          byteWords = B.unpack . toBS $ nid
          foldingFunc b i = b && (converted !! i == access byteWords i)
          access ws i = testBit (ws !! (i `div` 8)) (i `mod` 8)

-- | Checks wether fromByteStruct converts corretctly
fromByteStructCheck :: IdType -> Bool
fromByteStructCheck nid = nid == (fromByteStruct . toByteStruct $ nid)
