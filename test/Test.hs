{-|
Module      : Tests
Description : Tests for the modules

A few tests using QuickCheck and Tasty to make sure everything works
the way it's supposed to.
-}

module Main where

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit as HU

import Types
import Protocol
import Networking
import Tree
import Instance
import ReplyQueue
import Implementation

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [quickCheckTests, hUnitTests]

quickCheckTests = testGroup "QuickCheck" [
      typeChecks
    , protocolChecks
    , networkingChecks
    , treeChecks
    , instanceChecks
    , replyQueueChecks
    , implementationChecks
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
    , QC.testProperty "Expecting works the way it's supposed to"
         expectCheck
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
    , QC.testProperty "Finding closest works"
         findClosestCheck
    ]

instanceChecks = testGroup "Network.Kademlia.Instance" [
      QC.testProperty "Storing and Retrieving values works"
         storeAndFindValueCheck
    , QC.testProperty "Peers are put into the tree on first encounter"
         trackingKnownPeersCheck
    ]

replyQueueChecks = testGroup "Network.Kademlia.ReplyQueue" [
      QC.testProperty "Registering replies works"
          repliesCheck
    , QC.testProperty "Registrations are removed after being dispatched"
         removedCheck
    ]

implementationChecks = testGroup "Network.Kademlia.Implementation" [
      QC.testProperty "Joining the Network works"
         joinCheck
    , QC.testProperty "Storing and looking up values works"
        storeAndLookupCheck
    ]

hUnitTests = testGroup "HUnit" [
      instanceCases
    ]

instanceCases = testGroup "Network.Kademlia.Instance" [
      HU.testCase "PINGs are automaticly handled"
         handlesPingCheck
    ]
