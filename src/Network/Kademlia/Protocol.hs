{-|
Module      : Network.Kademlia.Protocol
Description : Implementation of the actual protocol

Netowrk.Kademlia.Protocol implements the parsing and serialisation of
ByteStrings into 'Protocol'-Values.
-}

module Network.Kademlia.Protocol
    ( serialize
    , parse
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Word (Word8)
import Data.List (foldl')

import Network.Kademlia.Networking
import Network.Kademlia.Types
import Network.Kademlia.Protocol.Parsing

-- | Retrieve the assigned protocolId
commandId :: Command a -> Word8
commandId PING             = 0
commandId (STORE _ _)      = 1
commandId (FIND_NODE _)    = 2
commandId (RETURN_NODES _) = 3
commandId (FIND_VALUE _)   = 4
commandId (RETURN_VALUE _) = 5

-- | Turn the command arguments into a ByteString
commandArgs :: (Show a) => Command a -> B.ByteString
commandArgs PING              = B.empty
commandArgs (STORE k v)       = k `B.append` C.pack (show v)
commandArgs (FIND_NODE id)    = id
commandArgs (FIND_VALUE k)    = k
commandArgs (RETURN_VALUE v)  = C.pack $ show v
commandArgs (RETURN_NODES kb) = foldl' B.append B.empty $ fmap peerToArg kb
    where peerToArg peer = C.pack $ peerHost peer ++ " " ++
                                    show (peerPort peer) ++ " "

-- | Turn a command into a sendable ByteString
serialize :: (Show a) => Id -> Command a -> B.ByteString
serialize id command = cId `B.cons` id `B.append` args
    where cId  = commandId command
          args = commandArgs command
