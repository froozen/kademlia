{-|
Module      : Network.Kademlia.Types
Description : Definitions of a few types

Network.Kademlia.Types defines a few types that are used throughout the
Netowrk.Kademlia codebase.
-}

module Network.Kademlia.Types
    ( Peer(..)
    , toPeer
    , toSockAddr
    , KBucket
    , Serialize(..)
    , Signal(..)
    , Command(..)
    ) where

import Network.Socket (SockAddr(..), PortNumber, inet_ntoa, inet_addr)
import Data.ByteString (ByteString)

-- | Representation of an UDP peer
data Peer = Peer {
      peerHost :: String
    , peerPort :: PortNumber
    } deriving (Eq, Ord, Show)

-- | Aliases to make the code more readable by using the same names as the
--   papers
type KBucket = [Peer]

-- | A structure serializable into and parsable from a ByteString
class Serialize a where
    fromBS :: ByteString -> Either String (a, ByteString)
    toBS :: a -> ByteString

-- | Try to convert a SockAddr to a Peer
toPeer :: SockAddr -> IO (Maybe Peer)
toPeer (SockAddrInet port host) = do
    hostname <- inet_ntoa host
    return $ Just $ Peer hostname port
toPeer _ = return Nothing

-- | Convert a Peer back to a SockAddr
toSockAddr :: Peer -> IO SockAddr
toSockAddr (Peer hostname port) = do
    host <- inet_addr hostname
    return $ SockAddrInet port host

-- | Representation of a protocl signal
data Signal i v = Signal {
      peerId :: i
    , peer :: Peer
    , command :: Command i v
    } deriving (Show, Eq)

-- | Representations of the different protocol commands
data Command i a = PING
                 | STORE        i a
                 | FIND_NODE    i
                 | RETURN_NODES KBucket
                 | FIND_VALUE   i
                 | RETURN_VALUE a
                   deriving (Eq, Show)
