{-|
Module      : Network.Kademlia.Networking
Description : All of the UDP network code

Network.Kademlia.Networking implements all the UDP network functionality.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.Kademlia.Networking
       ( KademliaHandle
       , kSock
       , replyQueue
       , openOn
       , openOnL
       , startRecvProcess
       , send
       , expect
       , closeK
       , logInfo
       , logError
       , logError'
       ) where

import           Control.Concurrent          (Chan, MVar, ThreadId, forkIO, isEmptyMVar,
                                              killThread, newChan, newEmptyMVar, readChan,
                                              takeMVar, tryPutMVar, writeChan, yield)
import           Control.Exception           (SomeException, catch, finally)
import           Control.Monad               (forM_, forever, unless, void)
import qualified Data.ByteString             as BS
import           Network.Socket              (AddrInfo (..), AddrInfoFlag (AI_PASSIVE),
                                              Socket, SocketOption (ReuseAddr),
                                              SocketType (Datagram), addrAddress,
                                              addrFlags, bind, close, defaultHints,
                                              defaultProtocol, getAddrInfo, Family(..),
                                              setSocketOption, socket, withSocketsDo)
import qualified Network.Socket.ByteString   as S
import           System.IO.Error             (ioError, userError)

import           Network.Kademlia.Config     (KademliaConfig (..), defaultConfig)
import           Network.Kademlia.Protocol   (parse, serialize)
import           Network.Kademlia.ReplyQueue (Reply (..), ReplyQueue (timeoutChan),
                                              ReplyRegistration, flush, register)
import           Network.Kademlia.Types      (Command, Peer (..), Serialize (..), toPeer)

-- | A handle to a UDP socket running the Kademlia connection
data KademliaHandle i a = KH {
      kSock      :: Socket
    , sendThread :: ThreadId
    , sendChan   :: Chan (Command i a, Peer)
    , replyQueue :: ReplyQueue i a
    , recvThread :: MVar ThreadId
    , logInfo    :: String -> IO ()
    , logError   :: String -> IO ()
    }

logError' :: KademliaHandle i a -> SomeException -> IO ()
logError' h = logError h . show

openOn
    :: (Show i, Serialize i, Serialize a)
    => String -> String -> i -> ReplyQueue i a -> IO (KademliaHandle i a)
openOn host port id' rq = openOnL host port id' lim rq (const $ pure ()) (const $ pure ())
  where
    lim = msgSizeLimit defaultConfig

-- | Open a Kademlia connection on specified port and return a corresponding
--   KademliaHandle
openOnL :: (Show i, Serialize i, Serialize a) => String -> String
       -> i -> Int -> ReplyQueue i a -> (String -> IO ()) -> (String -> IO ())
       -> IO (KademliaHandle i a)
openOnL host port id' lim rq logInfo logError = withSocketsDo $ do
    -- Get addr to bind to
    serveraddrs <- getAddrInfo
                 (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                 (Just host)
                 (Just port)

    -- TODO: support IPV6 by binding to two sockets
    let serveraddr = head $ filter (\a -> addrFamily a == AF_INET) serveraddrs

    -- Create socket and bind to it
    sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
    setSocketOption sock ReuseAddr 1
    bind sock (addrAddress serveraddr)

    chan <- newChan
    tId <- forkIO $ sendProcessL sock lim id' chan logInfo logError
    mvar <- newEmptyMVar

    -- Return the handle
    return $ KH sock tId chan rq mvar logInfo logError

sendProcessL
    :: (Show i, Serialize i, Serialize a)
    => Socket
    -> Int
    -> i
    -> Chan (Command i a, Peer)
    -> (String -> IO ())
    -> (String -> IO ())
    -> IO ()
sendProcessL sock lim nid chan logInfo logError =
    (withSocketsDo . forever . (`catch` logSomeError') . void $ do
        pair@(cmd, Peer host port) <- readChan chan

        logInfo $ "Send process: sending .. " ++ show pair ++ " (id " ++ show nid ++ ")"
        -- Get Peer's address
        (peeraddr:_) <- getAddrInfo Nothing (Just host)
                          (Just . show $ port)

        -- Send the signal
        case serialize lim nid cmd of
            Left err   -> logError err
            Right sigs -> forM_ sigs $ \sig -> S.sendTo sock sig (addrAddress peeraddr))
                -- Close socket on exception (ThreadKilled)
                `finally` close sock
  where
    logSomeError' :: SomeException -> IO ()
    logSomeError' e = logError $ "Caught error " ++ show e

-- | Dispatch the receiving process
--
--   Receive a signal and first try to dispatch it via the ReplyQueue. If that
--   fails, send it to the supplied default channel instead.
--
--   This throws an exception if called a second time.
startRecvProcess
    :: (Show i, Serialize i, Serialize a)
    => KademliaHandle i a
    -> IO ()
startRecvProcess kh = do
    tId <- forkIO $ (withSocketsDo . forever $ do
        -- Read from socket
        (received, addr) <- S.recvFrom (kSock kh) 1500
        -- Try to create peer
        peer <- toPeer addr
        case peer of
            Nothing -> logError kh ("Unknown peer " ++ show addr)
            Just p  ->
                -- Try parsing the signal
                case parse p received of
                    Left _    ->
                      logError kh ("Can't parse " ++ show (BS.length received) ++ " bytes from " ++ show peer)
                    Right sig -> do
                        logInfo kh ("Received signal " ++ show sig ++ " from " ++ show p)
                        -- Send the signal to the receivng process of instance
                        writeChan (timeoutChan . replyQueue $ kh) $ Answer sig
                        logInfo kh (" -- added from signal " ++ show p ++ " to chan")
        )
            -- Send Closed reply to all handlers
            `finally` do
                flush . replyQueue $ kh
                writeChan (timeoutChan . replyQueue $ kh) Closed

    success <- tryPutMVar (recvThread kh) tId
    unless success . ioError . userError $ "Receiving process already running"

-- | Send a Signal to a Peer over the connection corresponding to the
--   KademliaHandle
send :: KademliaHandle i a
     -> Peer
     -> Command i a
     -> IO ()
send kh peer cmd = writeChan (sendChan kh) (cmd, peer)

-- | Register a handler channel for a Reply
expect :: KademliaHandle i a
       -> ReplyRegistration i
       -> Chan (Reply i a)
       -> IO ()
expect kh reg = register reg . replyQueue $ kh

-- | Close the connection corresponding to a KademliaHandle
closeK :: KademliaHandle i a -> IO ()
closeK kh = do
    -- Kill recvThread
    empty <- isEmptyMVar . recvThread $ kh
    unless empty $ do
        tId <- takeMVar . recvThread $ kh
        killThread tId

    -- Kill sendThread
    killThread . sendThread $ kh

    close $ kSock kh
    yield
