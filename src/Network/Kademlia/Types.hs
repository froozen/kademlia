{-|
Module      : Network.Kademlia.Types
Description : Definitions of a few types

Network.Kademlia.Types defines a few types that are used throughout the
Netowrk.Kademlia codebase.
-}

module Network.Kademlia.Types
    ( Peer(..)
    , toPeer
    , KBucket
    , Id
    , Key
    , idSize
    , Signal(..)
    , Command(..)
    ) where

import Network.Socket (SockAddr(..), PortNumber, inet_ntoa)
import Data.ByteString (ByteString)

-- | Representation of an UDP peer
data Peer = Peer {
      peerHost :: String
    , peerPort :: PortNumber
    } deriving (Eq, Ord, Show)

-- | Aliases to make the code more readable by using the same names as the
--   papers
type KBucket = [Peer]
type Id    = ByteString
type Key   = Id

-- | A constant describing the length of an id
idSize :: Int
idSize = 32

-- | Try to convert a SockAddr to a Peer
toPeer :: SockAddr -> IO (Maybe Peer)
toPeer (SockAddrInet port host) = do
    hostname <- inet_ntoa host
    return $ Just $ Peer hostname port
toPeer _ = return Nothing

-- | Representation of a protocl signal
data Signal v = Signal {
      peerId :: Id
    , peer :: Peer
    , command :: Command v
    } deriving (Show)

-- | Representations of the different protocol commands
data Command a = PING
               | STORE        Key a
               | FIND_NODE    Id
               | RETURN_NODES KBucket
               | FIND_VALUE   Key
               | RETURN_VALUE a
                 deriving (Eq, Show)
