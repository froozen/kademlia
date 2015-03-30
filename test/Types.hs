{-|
Module      : Types
Description : Tests for Network.Kademlia.Types

Tests specific to Network.Kademlia.Types.
-}

module Types where

import Test.QuickCheck
import qualified Test.QuickCheck.Property as P

import qualified Data.ByteString as B
import Data.Bits (testBit)

import TestTypes
import Network.Kademlia.Types

-- | Checks wether toByteStruct converts corretctly
toByteStructCheck :: IdType -> Bool
toByteStructCheck id = foldl foldingFunc True [0..length converted - 1]
    where converted = toByteStruct id
          words = B.unpack . toBS $ id
          foldingFunc b i = b && (converted !! i == access words i)
          access ws i = testBit (ws !! (i `div` 8)) (i `mod` 8)

-- | Checks wether fromByteStruct converts corretctly
fromByteStructCheck :: IdType -> Bool
fromByteStructCheck id = toBS id == (fromByteStruct . toByteStruct $ id)
