{-|
Module      : Network.Kademlia.Types
Description : Definitions of a few types

Network.Kademlia.Types defines a few types that are used throughout the
Network.Kademlia codebase.
-}

{-# LANGUAGE DeriveGeneric #-}

module Network.Kademlia.Types
    ( Peer(..)
    , toPeer
    , Node(..)
    , KBucket
    , Signal(..)
    , Command(..)
    , serialize
    , parse
    , ByteStruct(..)
    , toByteStruct
    , fromByteStruct
    , distance
    ) where

import Network.Socket (SockAddr(..), PortNumber(..), inet_ntoa, inet_addr)
import qualified Data.ByteString.Lazy as B (ByteString, foldr, pack)
import Data.Bits (testBit, setBit, zeroBits)
import Data.Binary
import GHC.Generics (Generic)
import Control.Monad (liftM)

-- | Representation of an UDP peer
data Peer = Peer {
      peerHost :: String
    , peerPort :: PortNumber
    } deriving (Eq, Ord, Show, Generic)

-- | A custom Binary instance for PortNumber, as it doesn't have one
instance Binary PortNumber where
    put (PortNum w) = put w
    get = liftM PortNum get

instance Binary Peer

-- | Representation of a Kademlia Node, containing a Peer and an Id
data Node i = Node {
      peer :: Peer
    , nodeId :: i
    } deriving (Eq, Ord, Show, Generic)

instance (Binary i) => Binary (Node i)

-- | Aliases to make the code more readable by using the same names as the
--   papers
type KBucket i = [Node i]

-- | A Structure made up of bits, represented as a list of Bools
type ByteStruct = [Bool]

-- | Converts a Serialize into a ByteStruct
toByteStruct :: (Binary a) => a -> ByteStruct
toByteStruct s = B.foldr (\w bits -> convert w ++ bits) [] $ encode s
    where convert w = foldr (\i bits -> testBit w i : bits) [] [0..7]

-- | Convert a ByteStruct back to its ByteString form
fromByteStruct :: (Binary a) => ByteStruct -> a
fromByteStruct bs = decode s
    where s = B.pack . foldr (\i ws -> createWord i : ws) [] $ indexes
          indexes = [0..(length bs `div` 8) -1]
          createWord i = let pos = i * 8
                         in foldr changeBit zeroBits [pos..pos+7]

          changeBit i w = if bs !! i
                then setBit w (i `mod` 8)
                else w

-- Calculate the distance between two Ids, as specified in the Kademlia paper
distance :: (Binary i) => i -> i -> ByteStruct
distance idA idB = let bsA = toByteStruct idA
                       bsB = toByteStruct idB
                   in  zipWith xor bsA bsB
    where xor a b = not (a && b) && (a || b)

-- | Try to convert a SockAddr to a Peer
toPeer :: SockAddr -> IO (Maybe Peer)
toPeer (SockAddrInet port host) = do
    hostname <- inet_ntoa host
    return $ Just $ Peer hostname port
toPeer _ = return Nothing

-- | Representation of a protocl signal
data Signal i v = Signal {
      source :: Node i
    , command :: Command i v
    } deriving (Show, Eq, Generic)

instance (Binary i, Binary a) => Binary (Signal i a)

-- | Representations of the different protocol commands
data Command i a = PING
                 | PONG
                 | STORE        i a
                 | FIND_NODE    i
                 | RETURN_NODES i (KBucket i)
                 | FIND_VALUE   i
                 | RETURN_VALUE i a
                   deriving (Eq, Show, Generic)

instance (Binary i, Binary a) => Binary (Command i a)

-- | Packet to be sent over the Network
data KademliaPacket i a = KP i (Command i a) deriving (Generic)
instance (Binary i, Binary a) => Binary (KademliaPacket i a)

-- | Turn a command into a sendable ByteString
serialize :: (Binary i, Binary a) => i -> Command i a -> B.ByteString
serialize id = encode . KP id

-- | Parse a signal from a ByteString
--
--  (This needs to be supplied a Peer to be able to create a complete Signal)
parse :: (Binary i, Binary a) => Peer -> B.ByteString
      -> Either String (Signal i a)
parse peer bs = case decodeOrFail bs of
    Left  (_, _, err)    -> Left err
    Right (_, _, KP id cmd) -> Right . Signal (Node peer id) $ cmd
