{-|
Module      : Tests
Description : Tests for the modules

A few tests using QuickCheck and Tasty to make sure everything works
the way it's supposed to.
-}

module Main where

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Types
import Protocol
import Networking
import Tree

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [quickCheckTests]

quickCheckTests = testGroup "QuickCheck" [
      typeChecks
    , protocolChecks
    , networkingChecks
    , treeChecks
    ]

typeChecks = testGroup "Network.Kademlia.Types" [
      QC.testProperty "ByteString to ByteStruct conversion works"
         toByteStructCheck
    , QC.testProperty "ByteStruct to ByteString conversion works"
         fromByteStructCheck
    ]

protocolChecks = testGroup "Network.Kademlia.Protocol" [
      QC.testProperty "Protocol Serializing and Parsing works"
         parseCheck
    , QC.testProperty "Protocol messages are within the max UDP packet size"
         lengthCheck
    ]

networkingChecks = testGroup "Network.Kademlia.Networking" [
      QC.testProperty "Sending and Receiving works"
         sendCheck
    ]

treeChecks = testGroup "Network.Kademlia.Tree" [
      QC.testProperty "Inserting into the Tree works"
         insertCheck
    , QC.testProperty "Deleting from the Tree works"
         deleteCheck
    , QC.testProperty "Splitting works as expected"
         splitCheck
    , QC.testProperty "Buckets are within the size limit"
         bucketSizeCheck
    , QC.testProperty "Refreshing works as expected"
         refreshCheck
    ]
