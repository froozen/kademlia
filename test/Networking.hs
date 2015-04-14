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
import Control.Concurrent.Chan
import qualified Data.ByteString.Char8 as C

import TestTypes

-- | Make sure sending and receiving works
sendCheck = monadicIO $ do
    let pA = Peer "127.0.0.1" $ fromIntegral 1122
    let pB = Peer "127.0.0.1" $ fromIntegral 1123

    idA <- pick (arbitrary :: Gen IdType)
    idB <- pick (arbitrary :: Gen IdType)

    khA <- run $ openOn "1122" idA
    khB <- run $ openOn "1123" idB

    chan <- run $ (newChan :: IO (Chan (Reply IdType String)))
    run $ startRecvProcess khB chan

    cmd <- pick (arbitrary :: Gen (Command IdType String))

    run $ send khA pB cmd
    (Answer sig) <- run $ (readChan $ chan :: IO (Reply IdType String))

    run $ closeK khA
    run $ closeK khB

    assert $ command sig == cmd
    assert $ (peer . source $ sig) == pA
    assert $ (nodeId . source $ sig) == idA

    return ()

-- | Make sure expect works the way it's supposed to
expectCheck = monadicIO $ do
    let pA = Peer "127.0.0.1" $ fromIntegral 1122
    let pB = Peer "127.0.0.1" $ fromIntegral 1123

    idA <- pick (arbitrary :: Gen IdType)
    idB <- pick (arbitrary :: Gen IdType)

    cmd <- pick (arbitrary :: Gen (Command IdType String))

    let rtM = rType cmd
    pre $ rtM /= Nothing
    let (Just rt) = rtM
        rr = ReplyRegistration rt idA

    khA <- run $ openOn "1122" idA
    khB <- run $ openOn "1123" idB

    chanA <- run $ (newChan :: IO (Chan (Reply IdType String)))
    chanB <- run $ (newChan :: IO (Chan (Reply IdType String)))
    run $ startRecvProcess khB chanA

    run $ expect khB [rr] chanB
    run $ send khA pB cmd
    (Answer sig) <- run $ (readChan $ chanB :: IO (Reply IdType String))

    run $ closeK khA
    run $ closeK khB

    assert $ command sig == cmd
    assert $ (peer . source $ sig) == pA
    assert $ (nodeId . source $ sig) == idA

    return ()

-- | Convert a command into a ReplyType
rType :: Command i a -> Maybe (ReplyType i)
rType  PONG               = Just  R_PONG
rType (RETURN_VALUE id _) = Just (R_RETURN_VALUE id)
rType (RETURN_NODES id _) = Just (R_RETURN_NODES id)
rType _ = Nothing
