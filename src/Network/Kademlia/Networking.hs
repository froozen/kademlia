{-|
Module      : Network.Kademlia.Networking
Description : All of the UDP network code

Network.Kademlia.Networking implements all the UDP network functionality.
-}

module Network.Kademlia.Networking
    ( openOn
    , recv
    , send
    , closeK
    , KademliaHandle
    ) where

-- Just to make sure I'll only use the ByteString functions
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import qualified Network.Socket.ByteString as S
import Data.ByteString
import Control.Monad (forever)
import Control.Exception (finally)
import Control.Concurrent
import Control.Concurrent.Chan

import Network.Kademlia.Types
import Network.Kademlia.Protocol

-- | A handle to a UDP socket running the Kademlia connection
data KademliaHandle i a = KH {
      kSock      :: Socket
    , sendThread :: ThreadId
    , sendChan   :: Chan (Command i a, Peer)
    }

-- | Open a Kademlia connection on specified port and return a corresponding
--   KademliaHandle
openOn :: (Serialize i, Serialize a) => String -> i -> IO (KademliaHandle i a)
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

    -- Return the handle
    return $ KH sock tId chan

sendProcess :: (Serialize i, Serialize a) => Socket -> i
            -> Chan (Command i a, Peer) -> IO ()
sendProcess sock id chan = (withSocketsDo . forever $ do
    (cmd, Peer host port) <- readChan chan

    -- Get Peer's address
    (peeraddr:_) <- getAddrInfo Nothing (Just host)
                      (Just . show . fromIntegral $ port)

    -- Send the signal
    let sig = serialize id cmd
    S.sendTo sock sig (addrAddress peeraddr))
        -- | Close socket on exception (ThreadKilled)
        `finally` sClose sock

-- | Receive a signal from the connection corresponding to the specified
--   KademliaHandle
recv :: (Serialize i, Serialize a) =>  KademliaHandle i a -> IO (Signal i a)
recv kh = withSocketsDo $ do
    -- Read from socket
    (received, addr) <- S.recvFrom (kSock kh) 1500
    -- Try to create peer
    peer <- toPeer addr
    case peer of
        Nothing -> recv kh
        Just p  ->
            -- Try parsing the signal
            case parse p received of
                Left _    -> recv kh
                Right sig -> return sig

-- | Send a Signal to a Peer over the connection corresponding to the
--   KademliaHandle
send :: (Serialize i, Serialize a) => KademliaHandle i a -> Peer -> Command i a -> IO ()
send kh peer cmd = writeChan (sendChan kh) (cmd, peer)

-- | Close the connection corresponding to a KademliaHandle
closeK :: KademliaHandle i a -> IO ()
closeK kh = do
    killThread . sendThread $ kh
    yield
