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

-- | Checks wether toBinaryStructure converts corretctly
toBinaryStructureCheck :: IdType -> P.Result
toBinaryStructureCheck id = P.result { P.ok = Just ok }
    where converted = toBinaryStructure id
          words = B.unpack . toBS $ id
          ok = foldl foldingFunc True [0..length converted - 1]
          foldingFunc b i = b && (converted !! i == access words i)
          access ws i = testBit (ws !! (i `div` 8)) (i `mod` 8)
