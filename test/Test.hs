{-|
Module      : Tests
Description : Tests for the modules

A few tests using QuickCheck and Tasty to make sure everything works
the way it's supposed to.
-}

module Main where

import           Test.Tasty            (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit      as HU
import           Test.Tasty.QuickCheck as QC

import qualified HashNodeId            as Hash
import           Implementation        (idClashCheck, joinBannedCheck, joinCheck,
                                        joinFullCheck, lookupNodesCheck, nodeDownCheck,
                                        storeAndLookupCheck)
import           Instance              (banNodeCheck, handlesPingCheck, isNodeBannedCheck,
                                        snapshotCheck, storeAndFindValueCheck,
                                        trackingKnownPeersCheck)
import           Networking            (expectCheck, sendCheck)
import           Protocol              (lengthCheck, parseCheck)
import           ReplyQueue            (removedCheck, repliesCheck)
import           Tree                  (bucketSizeCheck, deleteCheck, findClosestCheck,
                                        insertCheck, pickupNotClosestDifferentCheck,
                                        refreshCheck, splitCheck, viewCheck)
import           Types                 (fromByteStructCheck, toByteStructCheck)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [quickCheckTests, hUnitTests]

quickCheckTests :: TestTree
quickCheckTests = testGroup "QuickCheck" [
      hashNodeChecks
    , typeChecks
    , protocolChecks
    , networkingChecks
    , treeChecks
    , instanceChecks
    , replyQueueChecks
    , implementationChecks
    ]

hashNodeChecks :: TestTree
hashNodeChecks = testGroup "Network.Kademlia.HashNodeId" [
      QC.testProperty "HashId to ByteString conversion works"
         Hash.toFromHashId
    , QC.testProperty "ByteString to HashId conversion works"
         Hash.fromToHashId
    , QC.testProperty "BadHashId to ByteString conversion does not work"
         Hash.toFromHashId
    , QC.testProperty "ByteString to BadHashId conversion does not work"
         Hash.fromToHashId
    , QC.testProperty "Successfully verifies a valid 'HashId'"
         Hash.verifyHashId
    , QC.testProperty "Unsuccessfully verifies an invalid 'HashId'"
         Hash.notVerifyBadHashId
    ]

typeChecks :: TestTree
typeChecks = testGroup "Network.Kademlia.Types" [
      QC.testProperty "ByteString to ByteStruct conversion works"
         toByteStructCheck
    , QC.testProperty "ByteStruct to ByteString conversion works"
         fromByteStructCheck
    ]

protocolChecks :: TestTree
protocolChecks = testGroup "Network.Kademlia.Protocol" [
      QC.testProperty "Protocol Serializing and Parsing works"
         parseCheck
    , QC.testProperty "Protocol messages are cut in pieces of required size"
         lengthCheck
    ]

networkingChecks :: TestTree
networkingChecks = testGroup "Network.Kademlia.Networking" [
      QC.testProperty "Sending and Receiving works"
         sendCheck
    , QC.testProperty "Expecting works the way it's supposed to"
         expectCheck
    ]

treeChecks :: TestTree
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
    , QC.testProperty "PickupRandom with closest provided doesn't contain closest"
         pickupNotClosestDifferentCheck
    , QC.testProperty "Getting view of tree works correctly"
         viewCheck
    ]

instanceChecks :: TestTree
instanceChecks = testGroup "Network.Kademlia.Instance" [
      QC.testProperty "Storing and Retrieving values works"
         storeAndFindValueCheck
    , QC.testProperty "Peers are put into the tree on first encounter"
         trackingKnownPeersCheck
    , HU.testCase "Setting ban and checking ban status works"
         isNodeBannedCheck
    , HU.testCase "Messages from banned node are ignored"
         banNodeCheck
    , QC.testProperty "Kademlia state snapshot is serialized and deserialized well"
         snapshotCheck
    ]

replyQueueChecks :: TestTree
replyQueueChecks = testGroup "Network.Kademlia.ReplyQueue" [
      QC.testProperty "Registering replies works"
          repliesCheck
    , QC.testProperty "Registrations are removed after being dispatched"
         removedCheck
    ]

implementationChecks :: TestTree
implementationChecks = testGroup "Network.Kademlia.Implementation" [
       QC.testProperty "Joining the Network works"
         joinCheck
     , QC.testProperty "Joining the Network full works"
         joinFullCheck
     , QC.testProperty "ID clashes are detected"
         idClashCheck
     , QC.testProperty "Join network to banned node works"
         joinBannedCheck
     , QC.testProperty "Storing and looking up values works"
        storeAndLookupCheck
     , QC.testProperty "Looking up Nodes works"
        lookupNodesCheck
    ]

hUnitTests :: TestTree
hUnitTests = testGroup "HUnit" [
      instanceCases
    , implementationCases
    ]

instanceCases :: TestTree
instanceCases = testGroup "Network.Kademlia.Instance" [
      HU.testCase "PINGs are automaticly handled"
         handlesPingCheck
    ]

implementationCases :: TestTree
implementationCases = testGroup "Network.Kademlia.Implementation" [
      HU.testCase "Trying to join over an offline Node is detected"
         nodeDownCheck
    ]
