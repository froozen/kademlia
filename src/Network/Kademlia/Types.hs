{-|
Module      : Network.Kademlia.Types
Description : Definitions of a few types

Network.Kademlia.Types defines a few types that are used throughout the
Netowrk.Kademlia codebase.
-}

module Network.Kademlia.Types
    ( Peer(..)
    , toPeer
    , Node(..)
    , KBucket
    , Serialize(..)
    , Signal(..)
    , Command(..)
    , ByteStruct(..)
    , toByteStruct
    , fromByteStruct
    ) where

import Network.Socket (SockAddr(..), PortNumber, inet_ntoa, inet_addr)
import qualified Data.ByteString as B (ByteString, foldr, pack)
import Data.Bits (testBit, setBit, zeroBits)

-- | Representation of an UDP peer
data Peer = Peer {
      peerHost :: String
    , peerPort :: PortNumber
    } deriving (Eq, Ord, Show)

-- | Representation of a Kademlia Node, containing a Peer and an Id
data Node i = Node {
      peer :: Peer
    , nodeId :: i
    } deriving (Eq, Ord, Show)

-- | Aliases to make the code more readable by using the same names as the
--   papers
type KBucket i = [Node i]

-- | A structure serializable into and parsable from a ByteString
class Serialize a where
    fromBS :: B.ByteString -> Either String (a, B.ByteString)
    toBS :: a -> B.ByteString

-- | A Structure made up of bits, represented as a list of Bools
type ByteStruct = [Bool]

-- | Converts a Serialize into a ByteStruct
toByteStruct :: (Serialize a) => a -> ByteStruct
toByteStruct s = B.foldr (\w bits -> convert w ++ bits) [] $ toBS s
    where convert w = foldr (\i bits -> testBit w i : bits) [] [0..7]

-- | Convert a ByteStruct back to its ByteString form
fromByteStruct :: ByteStruct -> B.ByteString
fromByteStruct bs = B.pack words
    where words = foldr (\i ws -> createWord i : ws) [] indexes
          indexes = [0..(length bs `div` 8) -1]
          createWord i = let pos = i * 8
                         in foldr changeBit zeroBits [pos..pos+7]

          changeBit i w = if bs !! i
                then setBit w (i `mod` 8)
                else w

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
    } deriving (Show, Eq)

-- | Representations of the different protocol commands
data Command i a = PING
                 | STORE        i a
                 | FIND_NODE    i
                 | RETURN_NODES (KBucket i)
                 | FIND_VALUE   i
                 | RETURN_VALUE a
                   deriving (Eq, Show)
