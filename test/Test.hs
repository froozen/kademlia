{-|
Module      : Tests
Description : Tests for the modules

A few tests using QuickCheck and Tasty to make sure everything works
the way it's supposed to.
-}

module Main where

import           Test.Tasty
import           Test.Tasty.HUnit      as HU
import           Test.Tasty.QuickCheck as QC

import           Implementation
import           Instance
import           Networking
import           Protocol
import           ReplyQueue
import           Tree
import           Types

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
    , QC.testProperty "Not closest doesn't contain closest"
         pickupNotClosestDifferentCheck
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
    , QC.testProperty "ID clashes are detected"
         idClashCheck
    , QC.testProperty "Storing and looking up values works"
        storeAndLookupCheck
    , QC.testProperty "Looking up Nodes works"
        lookupNodesCheck
    ]

hUnitTests = testGroup "HUnit" [
      instanceCases
    , implementationCases
    ]

instanceCases = testGroup "Network.Kademlia.Instance" [
      HU.testCase "PINGs are automaticly handled"
         handlesPingCheck
    ]

implementationCases = testGroup "Network.Kademlia.Implementation" [
      HU.testCase "Trying to join over an offline Node is detected"
         nodeDownCheck
    ]
