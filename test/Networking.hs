{-|
Module      : Networking
Description : Tests for Network.Kademlia.Networking

Tests specific to Network.Kademlia.Networking.
-}

module Networking where

import Test.QuickCheck
import Test.QuickCheck.Monadic

import Network.Kademlia.Networking
import Network.Kademlia.Types
import Network.Kademlia.ReplyQueue
import Control.Monad
import Control.Concurrent.Chan
import Control.Concurrent.STM
import qualified Data.ByteString.Char8 as C
import Data.Maybe (isJust)

import TestTypes

valueSet :: (Monad m) => PropertyM m (Peer, Peer, IdType, IdType)
valueSet = do
    let pA = Peer "127.0.0.1" 1122
        pB = Peer "127.0.0.1" 1123

    idA <- pick (arbitrary :: Gen IdType)
    idB <- pick (arbitrary :: Gen IdType)

    return (pA, pB, idA, idB)

-- | Make sure sending and receiving works
sendCheck = monadicIO $ do
    (pA, pB, idA, idB) <- valueSet

    rqA <- run emptyReplyQueue
    rqB <- run emptyReplyQueue

    khA <- run $ openOn "1122" idA rqA
    khB <- run $ (openOn "1123" idB rqB :: IO (KademliaHandle IdType String))

    run $ startRecvProcess khB

    cmd <- pick (arbitrary :: Gen (Command IdType String))

    run $ send khA pB cmd
    (Answer sig) <- run (readChan . timeoutChan $ rqB :: IO (Reply IdType String))

    run $ closeK khA
    run $ closeK khB

    assert $ command sig == cmd
    assert $ (peer . source $ sig) == pA
    assert $ (nodeId . source $ sig) == idA

    return ()

-- | Make sure expect works the way it's supposed to
expectCheck = monadicIO $ do
    (pA, pB, idA, idB) <- valueSet

    sig <- pick (arbitrary :: Gen (Signal IdType String))

    let rtM = rType . command $ sig
    pre . isJust $ rtM
    let (Just rt) = rtM
        rr = RR [rt] . nodeId . source $ sig

    rqA <- run emptyReplyQueue

    khA <- run $ openOn "1122" idA rqA

    run $ startRecvProcess khA

    testChan <- run (newChan :: IO (Chan (Reply IdType String)))
    run $ expect khA rr testChan
    run $ dispatch (Answer sig) rqA

    (Answer replySig) <- run (readChan testChan :: IO (Reply IdType String))

    run $ closeK khA

    assert $ replySig == sig

-- | Convert a command into a ReplyType
rType :: Command i a -> Maybe (ReplyType i)
rType  PONG               = Just  R_PONG
rType (RETURN_VALUE id _) = Just (R_RETURN_VALUE id)
rType (RETURN_NODES id _) = Just (R_RETURN_NODES id)
rType _ = Nothing
