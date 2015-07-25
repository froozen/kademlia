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
import Data.Maybe (isJust)

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

    rq <- emptyReplyQueue

    khA <- openOn "1122" idA rq :: IO (KademliaHandle IdType String)
    kiB <- create 1123 idB   :: IO (KademliaInstance IdType String)

    startRecvProcess khA

    send khA pB PING
    (Answer sig) <- readChan . timeoutChan $ rq :: IO (Reply IdType String)

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

    rq <- run emptyReplyQueue

    khA <- run $ openOn "1122" idA rq
    kiB <- run $ create 1123 idB :: PropertyM IO (KademliaInstance IdType String)

    run $ startRecvProcess khA

    run $ send khA pB $ STORE key value
    run $ send khA pB $ FIND_VALUE key

    -- There is a race condition, so the instance will sometimes try to store
    -- the value in the handle, before replying with a RETURN_VALUE
    (Answer sig) <- run (readChan . timeoutChan $ rq :: IO (Reply IdType String))
    sig <- case command sig of
            STORE _ _ -> do
                (Answer sig) <- run (readChan . timeoutChan $ rq :: IO (Reply IdType String))
                return sig
            _ -> return sig

    run $ closeK khA
    run $ close kiB

    let cmd = RETURN_VALUE key value :: Command IdType String

    monitor . counterexample $ "Commands inequal: " ++ show cmd ++ " /= " ++ show (command sig)
    assert $ cmd == command sig

    return ()

-- | Assert that a peer is put into the NodeTree on first encounter
trackingKnownPeersCheck :: Property
trackingKnownPeersCheck = monadicIO $ do
    let (_, pB) = peers
    (idA, idB) <- ids

    rq <- run emptyReplyQueue :: PropertyM IO (ReplyQueue IdType String)

    khA <- run $ openOn "1122" idA rq
    kiB <- run $ create 1123 idB :: PropertyM IO (KademliaInstance IdType String)

    run $ startRecvProcess khA

    run . send khA pB $ PING
    run . readChan . timeoutChan $ rq

    run $ closeK khA
    run $ close kiB

    node <- run $ lookupNode kiB idA
    assert . isJust $ node

    return ()
