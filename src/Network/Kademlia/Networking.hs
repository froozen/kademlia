{-|
Module      : Network.Kademlia.Networking
Description : All of the UDP network code

Network.Kademlia.Networking implements all the UDP network functionality.
-}

module Network.Kademlia.Networking
    ( openOn
    , sendTo
    ) where

-- Just to make sure I'll only use the ByteString functions
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import qualified Network.Socket.ByteString as S
import Data.ByteString
import Control.Monad (forever)
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan

import Network.Kademlia.Types
import Network.Kademlia.Protocol

-- | Open a listening UDP socket on a specified port and return a channel
--   delivering the incoming signals
openOn :: (Id i, Read a) => String -> IO (Chan (Signal i a))
openOn port = withSocketsDo $ do
    -- Get addr to bind to
    (serveraddr:_) <- getAddrInfo
                 (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                 Nothing (Just port)

    -- Create socket and bind to it
    sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
    bindSocket sock (addrAddress serveraddr)

    -- Create channel and start listening
    chan <- newChan
    forkIO $ listenOn sock chan

    return chan

-- | Function used with forkIO in 'openOn'
listenOn :: (Id i, Read a) =>  Socket -> Chan (Signal i a) -> IO ()
listenOn sock chan = forever $ do
    -- Read from socket
    (received, addr) <- S.recvFrom sock 1024
    -- Try to create peer
    peer <- toPeer addr
    case peer of
        Nothing -> return ()
        Just p  ->
            -- Try parsing the signal
            case parse p received of
                Left _    -> return ()
                Right sig -> writeChan chan sig

-- | Send a Signal to a Peer
sendTo :: (Id i, Show a) => Peer -> i -> Command i a -> IO ()
sendTo (Peer host port) id sig = withSocketsDo $ do
    -- Get Peer's address
    (peeraddr:_) <- getAddrInfo Nothing (Just host)
                      (Just . show . fromIntegral $ port)
    -- Create socket
    sock <- socket (addrFamily peeraddr) Datagram defaultProtocol

    -- Send the signal
    S.sendTo sock (serialize id sig) (addrAddress peeraddr)
    return ()
