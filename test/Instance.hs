{-|
Module      : Instance
Description : Tests for Network.Kademlia.Instance

Tests specific to Network.Kademlia.Instance.
-}

module Instance
       ( handlesPingCheck
       , storeAndFindValueCheck
       , trackingKnownPeersCheck
       , isNodeBannedCheck
       , banNodeCheck
       ) where


import           Control.Concurrent          (forkIO, threadDelay)
import           Control.Concurrent.Chan     (readChan, writeChan)
import           Control.Concurrent.STM      (atomically, newTVarIO, readTVarIO,
                                              writeTVar)
import           Control.Monad               (liftM2, void)
import qualified Data.ByteString.Char8       as C
import           Data.Maybe                  (fromJust, isJust)

import           Test.HUnit                  (Assertion, assertEqual, assertFailure)
import           Test.QuickCheck             (Property, arbitrary, counterexample)
import           Test.QuickCheck.Monadic     (PropertyM, assert, monadicIO, monitor, pick,
                                              run)

import           Network.Kademlia            (close, create)
import           Network.Kademlia.Instance   (KademliaInstance (..), banNode, dumpPeers,
                                              isNodeBanned, lookupNode)
import           Network.Kademlia.Networking (KademliaHandle (..), closeK, openOn, send,
                                              startRecvProcess)
import           Network.Kademlia.ReplyQueue (Reply (..), ReplyQueue (..),
                                              emptyReplyQueue)
import           Network.Kademlia.Types      (Command (..), Node (..), Peer (..),
                                              Serialize (..), Signal (..), command)

import           TestTypes                   (IdType (..))

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
    let (_, pB) = peers

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
    let (_, pB) = peers
    (idA, idB) <- ids

    receivedCmd <- run $ do
        rq <- emptyReplyQueue

        khA <- openOn "1122" idA rq
        kiB <- create 1123 idB :: IO (KademliaInstance IdType String)

        startRecvProcess khA

        send khA pB $ STORE key value
        send khA pB $ FIND_VALUE key

        -- There is a race condition, so the instance will sometimes try to store
        -- the value in the handle, before replying with a RETURN_VALUE
        (Answer sig) <- readChan . timeoutChan $ rq :: IO (Reply IdType String)
        cmdSig <- case command sig of
                STORE _ _ -> do
                    (Answer asig) <- readChan . timeoutChan $ rq :: IO (Reply IdType String)
                    return asig
                _ -> return sig

        closeK khA
        close kiB

        return . command $ cmdSig

    let cmd = RETURN_VALUE key value :: Command IdType String

    monitor . counterexample $ "Commands inequal: " ++ show cmd ++ " /= " ++ show receivedCmd
    assert $ cmd == receivedCmd

    return ()

-- | Assert that a peer is put into the NodeTree on first encounter
trackingKnownPeersCheck :: Property
trackingKnownPeersCheck = monadicIO $ do
    let (_, pB) = peers
    (idA, idB) <- ids

    (node, kiB) <- run $ do
        rq <- emptyReplyQueue :: IO (ReplyQueue IdType String)

        khA <- openOn "1122" idA rq
        kiB <- create 1123 idB :: IO (KademliaInstance IdType String)

        startRecvProcess khA

        send khA pB $ PING
        () <$ readChan (timeoutChan rq)

        node <- lookupNode kiB idA

        closeK khA
        close kiB

        return (node, kiB)

    assert . isJust $ node

    nodes <- run . dumpPeers $ kiB
    assert $ nodes == [fromJust node]

    return ()

-- | Make sure `isNodeBanned` works correctly
isNodeBannedCheck :: Assertion
isNodeBannedCheck = do
    inst <- create 1123 idA :: IO (KademliaInstance IdType String)
    let check msg ans = do
            ban <- isNodeBanned inst idB
            assertEqual msg ban ans

    check "Initial" False

    banNode inst idB $ return True
    check "Plain ban set" True

    banValue <- newTVarIO False
    banNode inst idB $ readTVarIO banValue
    check "Reset ban to False" False

    atomically $ writeTVar banValue True
    check "Change 'banned' value via TVar" False

    close inst

    where idA = IT . C.pack $ "hello"
          idB = IT . C.pack $ "herro"

-- | Messages from banned node are ignored
banNodeCheck :: Assertion
banNodeCheck = do
    let (_, pB) = peers

    let (Right (idA, _)) = fromBS . C.replicate 32 $ 'a'
                           :: Either String (IdType, C.ByteString)
    let (Right (idB, _)) = fromBS . C.replicate 32 $ 'b'
                           :: Either String (IdType, C.ByteString)

    rq <- emptyReplyQueue

    khA <- openOn "1122" idA rq :: IO (KademliaHandle IdType String)
    kiB <- create 1123 idB   :: IO (KademliaInstance IdType String)

    banNode kiB idA $ return True
    startRecvProcess khA

    send khA pB PING

    -- if no message received for long enough, put OK message
    void . forkIO $ do
        threadDelay 10000
        writeChan (timeoutChan rq) Closed

    res <- readChan . timeoutChan $ rq :: IO (Reply IdType String)

    closeK khA
    close kiB

    case res of
        Closed -> return ()
        _     -> assertFailure "Message from banned node isn't ignored"

    return ()
