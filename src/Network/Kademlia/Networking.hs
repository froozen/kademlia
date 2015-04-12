{-|
Module      : Network.Kademlia.Networking
Description : All of the UDP network code

Network.Kademlia.Networking implements all the UDP network functionality.
-}

module Network.Kademlia.Networking
    ( openOn
    , startRecvProcess
    , send
    , closeK
    , KademliaHandle
    ) where

-- Just to make sure I'll only use the ByteString functions
import Network.Socket hiding (send, sendTo, recv, recvFrom, Closed)
import qualified Network.Socket.ByteString as S
import Data.ByteString.Lazy
import Control.Monad (forever, unless)
import Control.Exception (finally)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import System.IO.Error (ioError, userError)
import Data.Binary

import Network.Kademlia.Types
import Network.Kademlia.ReplyQueue

-- | A handle to a UDP socket running the Kademlia connection
data KademliaHandle i a = KH {
      kSock      :: Socket
    , sendThread :: ThreadId
    , sendChan   :: Chan (Command i a, Peer)
    , replyQueue :: ReplyQueue i a
    , recvThread :: MVar ThreadId
    }

-- | Open a Kademlia connection on specified port and return a corresponding
--   KademliaHandle
openOn :: (Binary i, Binary a) => String -> i -> IO (KademliaHandle i a)
openOn port id = withSocketsDo $ do
    -- Get addr to bind to
    (serveraddr:_) <- getAddrInfo
                 (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                 Nothing (Just port)

    -- Create socket and bind to it
    sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
    bindSocket sock (addrAddress serveraddr)

    chan <- newChan
    tId <- forkIO . sendProcess sock id $ chan
    rq <- atomically emptyReplyQueue
    mvar <- newEmptyMVar

    -- Return the handle
    return $ KH sock tId chan rq mvar

sendProcess :: (Binary i, Binary a) => Socket -> i
            -> Chan (Command i a, Peer) -> IO ()
sendProcess sock id chan = (withSocketsDo . forever $ do
    (cmd, Peer host port) <- readChan chan

    -- Get Peer's address
    (peeraddr:_) <- getAddrInfo Nothing (Just host)
                      (Just . show . fromIntegral $ port)

    -- Send the signal
    let sig = toStrict . serialize id $ cmd
    S.sendTo sock sig (addrAddress peeraddr))
        -- | Close socket on exception (ThreadKilled)
        `finally` sClose sock

-- | Dispatch the receiving process
--
--   Receive a signal and first try to dispatch it via the ReplyQueue. If that
--   fails, send it to the supplied default channel instead.
--
--   This throws an exception if called a second time.
startRecvProcess :: (Binary i, Binary a, Eq i, Eq a) => KademliaHandle i a
                 -> TChan (Reply i a) -> IO ()
startRecvProcess kh defaultChan = do
    tId <- forkIO $ (withSocketsDo . forever $ do
        -- Read from socket
        (received, addr) <- S.recvFrom (kSock kh) 1500
        -- Try to create peer
        peer <- toPeer addr
        case peer of
            Nothing -> return ()
            Just p  ->
                -- Try parsing the signal
                case parse p . fromStrict $ received of
                    Left _    -> return ()
                    Right sig -> do
                        -- Try to dispatch the signal
                        success <- atomically . dispatch sig $ replyQueue kh

                        unless success $
                            -- Send it to the default channel
                            atomically . writeTChan defaultChan $ Answer sig)

            -- Send Closed reply to all handlers
            `finally` atomically (do
                flush . replyQueue $ kh
                writeTChan defaultChan  Closed)

    success <- tryPutMVar (recvThread kh) tId
    unless success . ioError . userError $ "Receiving process already running"

-- | Send a Signal to a Peer over the connection corresponding to the
--   KademliaHandle
send :: (Binary i, Binary a) => KademliaHandle i a -> Peer -> Command i a
     -> IO ()
send kh peer cmd = writeChan (sendChan kh) (cmd, peer)

-- | Close the connection corresponding to a KademliaHandle
closeK :: KademliaHandle i a -> IO ()
closeK kh = do
    -- | Kill sendThread
    killThread . sendThread $ kh

    -- | Kill recvThread
    empty <- isEmptyMVar . recvThread $ kh
    unless empty $ do
        tId <- takeMVar . recvThread $ kh
        killThread tId

    yield
