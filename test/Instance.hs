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
import Control.Concurrent.Chan
import Control.Monad (liftM2)

import TestTypes

-- | The default set of peers
peers :: (Peer, Peer)
peers = let pA = Peer "127.0.0.1" 1122
            pB = Peer "127.0.0.1" 1123
        in (pA, pB)

-- | A set of randomly generated Ids
ids :: (Monad m) => PropertyM m (IdType, IdType)
ids = liftM2 (,) (pick arbitrary) (pick arbitrary)

-- | Checks wether PINGs are handled appropriately
handlesPingCheck :: Assertion
handlesPingCheck = do
    let (pA, pB) = peers

    let (Right (idA, _)) = fromBS . C.replicate 32 $ 'a'
                           :: Either String (IdType, C.ByteString)
    let (Right (idB, _)) = fromBS . C.replicate 32 $ 'b'
                           :: Either String (IdType, C.ByteString)

    khA <- openOn "1122" idA :: IO (KademliaHandle IdType String)
    kiB <- create 1123 idB   :: IO (KademliaInstance IdType String)

    chan <- newChan
    startRecvProcess khA chan

    send khA pB PING
    (Answer sig) <- readChan chan :: IO (Reply IdType String)

    closeK khA
    close kiB

    assertEqual "" (command sig) PONG
    assertEqual "" (peer . source $ sig) pB
    assertEqual "" (nodeId . source $ sig) idB

    return ()

-- | Make sure a stored value can be retrieved
storeAndFindValueCheck :: IdType -> String -> Property
storeAndFindValueCheck key value = monadicIO $ do
    let (pA, pB) = peers
    (idA, idB) <- ids

    khA <- run $ openOn "1122" idA
    kiB <- run $ create 1123 idB :: PropertyM IO (KademliaInstance IdType String)

    chan <- run newChan
    run $ startRecvProcess khA chan

    run $ send khA pB $ STORE key value
    run $ send khA pB $ FIND_VALUE key

    (Answer sig) <- run (readChan chan :: IO (Reply IdType String))

    run $ closeK khA
    run $ close kiB

    let cmd = RETURN_VALUE key value :: Command IdType String

    monitor . counterexample $ "Commands inequal: " ++ show cmd ++ " /= " ++ show (command sig)
    assert $ cmd == command sig

    return ()
