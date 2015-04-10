{-|
Module      : Network.Kademlia.Protocol
Description : Implementation of the actual protocol

Network.Kademlia.Protocol implements the parsing and serialisation of
ByteStrings into 'Protocol'-Values.
-}

module Network.Kademlia.Protocol
    ( serialize
    , parse
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Builder (toLazyByteString, word16BE)
import Data.Word (Word8)
import Data.List (foldl')

import Network.Kademlia.Types
import Network.Kademlia.Protocol.Parsing

-- | Retrieve the assigned protocolId
commandId :: Command i a -> Word8
commandId PING               = 0
commandId PONG               = 1
commandId (STORE _ _)        = 2
commandId (FIND_NODE _)      = 3
commandId (RETURN_NODES _ _) = 4
commandId (FIND_VALUE _)     = 5
commandId (RETURN_VALUE _ _) = 6

-- | Turn the command arguments into a ByteString
commandArgs :: (Serialize i, Serialize a) => Command i a -> B.ByteString
commandArgs PING                 = B.empty
commandArgs PONG                 = B.empty
commandArgs (STORE k v)          = toBS k `B.append` toBS v
commandArgs (FIND_NODE id)       = toBS id
commandArgs (FIND_VALUE k)       = toBS k
commandArgs (RETURN_VALUE id v)  = toBS id `B.append` toBS v
commandArgs (RETURN_NODES id kb) = toBS id `B.append`
                                   foldl' B.append B.empty (fmap nodeToArg kb)

nodeToArg :: (Serialize i) => Node i -> B.ByteString
nodeToArg node = id `B.append` C.pack (host ++ " ") `B.append` port
    where id = toBS . nodeId $ node
          host = peerHost . peer $ node
          port = toBinary . fromIntegral . peerPort . peer $ node
          -- Converts a Word16 into a two character ByteString
          toBinary = B.concat . L.toChunks . toLazyByteString . word16BE

-- | Turn a command into a sendable ByteString
serialize :: (Serialize i, Serialize a) => i -> Command i a -> B.ByteString
serialize id command = cId `B.cons` toBS id `B.append` args
    where cId  = commandId command
          args = commandArgs command
