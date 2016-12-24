{-|
Module      : Networking
Description : Tests for Network.Kademlia.Networking

Tests specific to Network.Kademlia.Networking.
-}

module Networking
       ( expectCheck
       , sendCheck
       ) where


import           Control.Concurrent.Chan     (Chan, newChan, readChan)
import           Data.Maybe                  (isJust)
import           Test.QuickCheck             (Gen, Property, arbitrary)
import           Test.QuickCheck.Monadic     (PropertyM, assert, monadicIO, pick, pre,
                                              run)

import           Network.Kademlia.Networking (KademliaHandle (..), closeK, expect, openOn,
                                              send, startRecvProcess)
import           Network.Kademlia.ReplyQueue (Reply (..), ReplyQueue (..),
                                              ReplyRegistration (RR), ReplyType (..),
                                              dispatch, emptyReplyQueue)
import           Network.Kademlia.Types      (Command (..), Node (..), Peer (..),
                                              Signal (..))

import           TestTypes                   (IdType (..))

valueSet :: (Monad m) => PropertyM m (Peer, Peer, IdType, IdType)
valueSet = do
    let pA = Peer "127.0.0.1" 1122
        pB = Peer "127.0.0.1" 1123

    idA <- pick (arbitrary :: Gen IdType)
    idB <- pick (arbitrary :: Gen IdType)

    return (pA, pB, idA, idB)

-- | Make sure sending and receiving works
sendCheck :: Command IdType String -> Property
sendCheck cmd = monadicIO $ do
    (pA, pB, idA, idB) <- valueSet

    sig <- run $ do
        rqA <- emptyReplyQueue
        rqB <- emptyReplyQueue

        khA <- openOn "1122" idA rqA
        khB <- (openOn "1123" idB rqB
                    :: IO (KademliaHandle IdType String))

        startRecvProcess khB

        send khA pB cmd
        (Answer sig) <- readChan . timeoutChan $ rqB :: IO (Reply IdType String)

        closeK khA
        closeK khB

        return sig

    assert $ command sig == cmd
    assert $ (peer . source $ sig) == pA
    assert $ (nodeId . source $ sig) == idA

    return ()

-- | Make sure expect works the way it's supposed to
expectCheck :: Signal IdType String -> IdType -> Property
expectCheck sig idA = monadicIO $ do
    let rtM = rType . command $ sig
    pre . isJust $ rtM
    let (Just rt) = rtM
        rr = RR [rt] . nodeId . source $ sig

    replySig <- run $ do
        rqA <- emptyReplyQueue

        khA <- openOn "1122" idA rqA

        startRecvProcess khA

        testChan <- newChan :: IO (Chan (Reply IdType String))
        expect khA rr testChan
        dispatch (Answer sig) rqA

        (Answer replySig) <- readChan testChan :: IO (Reply IdType String)

        closeK khA

        return replySig

    assert $ replySig == sig

-- | Convert a command into a ReplyType
rType :: Command i a -> Maybe (ReplyType i)
rType  PONG                  = Just  R_PONG
rType (RETURN_VALUE nid _)   = Just (R_RETURN_VALUE nid)
rType (RETURN_NODES _ nid _) = Just (R_RETURN_NODES nid)
rType _                      = Nothing
