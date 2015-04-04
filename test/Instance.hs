{-|
Module      : Instance
Description : Tests for Network.Kademlia.Instance

Tests specific to Network.Kademlia.Instance.
-}

module Instance where

import Test.HUnit

import Network.Kademlia.Instance
import Network.Kademlia
import Network.Kademlia.Networking
import Network.Kademlia.Types
import qualified Data.ByteString.Char8 as C

import TestTypes

-- | Checks wether PINGs are handled appropriately
handlesPingCheck :: Assertion
handlesPingCheck = do
    let pA = Peer "127.0.0.1" $ fromIntegral 1122
    let pB = Peer "127.0.0.1" $ fromIntegral 1123

    let (Right (idA, _)) = fromBS . C.replicate 32 $ 'a'
                           :: Either String (IdType, C.ByteString)
    let (Right (idB, _)) = fromBS . C.replicate 32 $ 'b'
                           :: Either String (IdType, C.ByteString)

    khA <- openOn "1122" idA :: IO (KademliaHandle IdType String)
    kiB <- create 1123 idB   :: IO (KademliaInstance IdType String)

    send khA pB PING
    sig <- recv khA :: IO (Signal IdType String)

    assertEqual "" (command sig) PONG
    assertEqual "" (peer . source $ sig) pB
    assertEqual "" (nodeId . source $ sig) idB

    closeK khA
    close kiB

    return ()

-- | Checks wether FIND_NODE is handled appropriately and is deterministic
handlesFindNodeCheck :: Assertion
handlesFindNodeCheck = do
    let pA = Peer "127.0.0.1" $ fromIntegral 1122
    let pB = Peer "127.0.0.1" $ fromIntegral 1123

    let (Right (idA, _)) = fromBS . C.replicate 32 $ 'a'
                           :: Either String (IdType, C.ByteString)
    let (Right (idB, _)) = fromBS . C.replicate 32 $ 'b'
                           :: Either String (IdType, C.ByteString)

    khA <- openOn "1122" idA :: IO (KademliaHandle IdType String)
    kiB <- create 1123 idB   :: IO (KademliaInstance IdType String)

    send khA pB $ FIND_NODE idA
    sig1 <- recv khA :: IO (Signal IdType String)

    send khA pB $ FIND_NODE idA
    sig2 <- recv khA :: IO (Signal IdType String)

    assertEqual "" sig1 sig2

    closeK khA
    close kiB

    return ()
