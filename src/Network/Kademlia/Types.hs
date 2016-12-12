{-|
Module      : Network.Kademlia.Types
Description : Definitions of a few types

Network.Kademlia.Types defines a few types that are used throughout the
Network.Kademlia codebase.
-}

module Network.Kademlia.Types
    ( Peer(..)
    , toPeer
    , Node(..)
    , sortByDistanceTo
    , Serialize(..)
    , Signal(..)
    , Command(..)
    , ByteStruct(..)
    , toByteStruct
    , fromByteStruct
    , distance
    ) where

import           Data.Bits       (setBit, testBit, zeroBits)
import qualified Data.ByteString as B (ByteString, foldr, pack)
import           Data.Function   (on)
import           Data.List       (sortBy)
import           Network.Socket  (PortNumber, SockAddr (..), inet_addr, inet_ntoa)

-- | Representation of an UDP peer
data Peer = Peer {
      peerHost :: String
    , peerPort :: PortNumber
    } deriving (Eq, Ord)

instance Show Peer where
  show (Peer h p) = h ++ ":" ++ show p

-- | Representation of a Kademlia Node, containing a Peer and an Id
data Node i = Node {
      peer   :: Peer
    , nodeId :: i
    } deriving (Eq, Ord)

instance Show i => Show (Node i) where
  show (Node peer nodeId) = show peer ++ " (" ++ show nodeId ++ ")"

-- | Sort a bucket by the closeness of its nodes to a give Id
sortByDistanceTo :: (Serialize i) => [Node i] -> i -> [Node i]
sortByDistanceTo bucket id = unpack . sort . pack $ bucket
    where pack bk = zip bk $ map f bk
          f = distance id . nodeId
          sort = sortBy (compare `on` snd)
          unpack = map fst

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
fromByteStruct :: (Serialize a) => ByteStruct -> a
fromByteStruct bs = case fromBS s of
                    (Right (converted, _)) -> converted
                    (Left err) -> error $ "Failed to convert from ByteStruct: " ++ err
    where s = B.pack . foldr (\i ws -> createWord i : ws) [] $ indexes
          indexes = [0..(length bs `div` 8) -1]
          createWord i = let pos = i * 8
                         in foldr changeBit zeroBits [pos..pos+7]

          changeBit i w = if bs !! i
                then setBit w (i `mod` 8)
                else w

-- Calculate the distance between two Ids, as specified in the Kademlia paper
distance :: (Serialize i) => i -> i -> ByteStruct
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
      source  :: Node i
    , command :: Command i v
    } deriving (Show, Eq)

-- | Representations of the different protocol commands
data Command i a = PING
                 | PONG
                 | STORE        i a
                 | FIND_NODE    i
                 | RETURN_NODES i [Node i]
                 | FIND_VALUE   i
                 | RETURN_VALUE i a
                   deriving (Eq)

instance Show i => Show (Command i a) where
  show PING                   = "PING"
  show PONG                   = "PONG"
  show (STORE        i a)     = "STORE " ++ show i ++ " <data>"
  show (FIND_NODE    i)       = "FIND_NODE " ++ show i
  show (RETURN_NODES i nodes) = "RETURN_VALUE " ++ show i ++ " " ++ show nodes
  show (FIND_VALUE   i)       = "FIND_VALUE " ++ show i
  show (RETURN_VALUE i _)     = "RETURN_VALUE " ++ show i ++ " <data>"
