{-|
Module      : Instance
Description : Tests for Network.Kademlia.Instance

Tests specific to Network.Kademlia.Instance.
-}

module Instance where

import Test.HUnit hiding (assert)
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Network.Kademlia.Instance
import Network.Kademlia
import Network.Kademlia.Networking
import Network.Kademlia.Types
import Network.Kademlia.ReplyQueue
import qualified Data.ByteString.Char8 as C
import Control.Concurrent.STM

import TestTypes

-- | Checks wether PINGs are handled appropriately
handlesPingCheck :: Assertion
handlesPingCheck = do
    let pA = Peer "127.0.0.1" $ fromIntegral 1122
    let pB = Peer "127.0.0.1" $ fromIntegral 1123

    let idA = IT . C.replicate 32 $ 'a'
    let idB = IT . C.replicate 32 $ 'b'

    khA <- openOn "1122" idA :: IO (KademliaHandle IdType String)
    kiB <- create 1123 idB   :: IO (KademliaInstance IdType String)

    chan <- newTChanIO
    startRecvProcess khA chan

    send khA pB PING
    (Answer sig) <- atomically . readTChan $ chan :: IO (Reply IdType String)

    assertEqual "" (command sig) PONG
    assertEqual "" (peer . source $ sig) pB
    assertEqual "" (nodeId . source $ sig) idB

    closeK khA
    close kiB

    return ()

-- | Checks wether FIND_NODE is handled appropriately and is deterministic
handlesFindNodeCheck :: Property
handlesFindNodeCheck = monadicIO $ do
    let pA = Peer "127.0.0.1" $ fromIntegral 1122
    let pB = Peer "127.0.0.1" $ fromIntegral 1123

    idA <- pick (arbitrary :: Gen IdType)
    idB <- pick (arbitrary :: Gen IdType)

    khA <- run $ (openOn "1122" idA :: IO (KademliaHandle IdType String))
    kiB <- run $ (create 1123 idB   :: IO (KademliaInstance IdType String))

    chan <- run $ newTChanIO
    run $ startRecvProcess khA chan

    run $ send khA pB $ FIND_NODE idA
    (Answer sig1) <- run $ (atomically . readTChan $ chan
                                :: IO (Reply IdType String))

    run $ send khA pB $ FIND_NODE idA
    (Answer sig2) <- run $ (atomically . readTChan $ chan
                                :: IO (Reply IdType String))

    run $ closeK khA
    run $ close kiB

    monitor . counterexample $ "Signals inequal: " ++ show sig1 ++ "\n  /=\n" ++ show sig2
    assert $ sig1 == sig2

    return ()

-- | Make sure a stored value can be retrieved
storeAndFindValueCheck :: IdType -> String -> Property
storeAndFindValueCheck key value = monadicIO $ do
    let pA = Peer "127.0.0.1" $ fromIntegral 1122
    let pB = Peer "127.0.0.1" $ fromIntegral 1123

    idA <- pick (arbitrary :: Gen IdType)
    idB <- pick (arbitrary :: Gen IdType)

    khA <- run $ openOn "1122" idA
    kiB <- run $ create 1123 idB :: PropertyM IO (KademliaInstance IdType String)

    chan <- run $ newTChanIO
    run $ startRecvProcess khA chan

    run $ send khA pB $ STORE key value
    run $ send khA pB $ FIND_VALUE key

    (Answer sig) <- run $ (atomically . readTChan $ chan
                                :: IO (Reply IdType String))

    run $ closeK khA
    run $ close kiB

    let cmd = RETURN_VALUE key value :: Command IdType String

    monitor . counterexample $ "Commands inequal: " ++ show cmd ++ " /= " ++ show (command sig)
    assert $ cmd == command sig

    return ()
