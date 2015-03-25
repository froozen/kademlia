{-|
Module      : Network.Kademlia.Networking
Description : All of the UDP network code

Network.Kademlia.Networking implements all the UDP network functionality.
-}

module Network.Kademlia.Networking
    ( openOn
    ) where

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
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
    (received, addr) <- recvFrom sock 1024
    -- Try to create peer
    peer <- toPeer addr
    case peer of
        Nothing -> return ()
        Just p  ->
            -- Try parsing the signal
            case parse p received of
                Left _    -> return ()
                Right sig -> writeChan chan sig
