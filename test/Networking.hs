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
import qualified Data.ByteString.Char8 as C

import Types

-- | Make sure sending and receiving works
sendCheck = monadicIO $ do
    let pA = Peer "127.0.0.1" $ fromIntegral 1122
    let pB = Peer "127.0.0.1" $ fromIntegral 1123

    idA <- pick (arbitrary :: Gen IdType)
    idB <- pick (arbitrary :: Gen IdType)

    khA <- run $ openOn "1122" idA
    khB <- run $ openOn "1123" idB

    cmd <- pick (arbitrary :: Gen (Command IdType String))

    run $ send khA pB cmd
    sig <- run $ (recv khB :: IO (Signal IdType String))

    assert $ command sig == cmd
    assert $ peer sig == pA
    assert $ peerId sig == idA

    run $ closeK khA
    run $ closeK khB

    return ()
