{-|
Module      : Network.Kademlia.Protocol
Description : Implementation of the actual protocol

Network.Kademlia.Protocol implements the parsing and serialisation of
ByteStrings into 'Protocol'-Values.
-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ViewPatterns    #-}

module Network.Kademlia.Protocol
    ( serialize
    , parse
    ) where

import           Control.Monad                     (when)
import           Control.Monad.Except              (throwError)
import qualified Data.ByteString                   as B
import           Data.ByteString.Builder           (toLazyByteString, word16BE)
import qualified Data.ByteString.Char8             as C
import qualified Data.ByteString.Lazy              as L
import           Data.List                         (scanl')
import           Data.Word                         (Word8)

import           Network.Kademlia.Protocol.Parsing (parse)
import           Network.Kademlia.Types            (Command (..), Node (..),
                                                    Serialize (..), peerHost, peerPort)

-- | Retrieve the assigned protocolId
commandId :: Command i a -> Word8
commandId PING                 = 0
commandId PONG                 = 1
commandId (STORE _ _)          = 2
commandId (FIND_NODE _)        = 3
commandId (RETURN_NODES _ _ _) = 4
commandId (FIND_VALUE _)       = 5
commandId (RETURN_VALUE _ _)   = 6

-- | Turn the command arguments into a ByteString, which fits into specified size.
-- Remaining part of command would be also returned, if any.
serialize :: (Serialize i, Serialize a)
            => Int -> i -> Command i a -> Either String [B.ByteString]
serialize size_ (toBS -> myId) cmd@(RETURN_NODES _ (toBS -> nid) allNodes) =
    addPrefix <$> genPacks allNodes
  where
    addPrefix packs = map (prefix n `B.append`) packs
      where n = fromIntegral $ length packs
    prefixSize = size_ - 2 - B.length myId - B.length nid
    prefix :: Word8 -> B.ByteString
    prefix n = (commandId cmd) `B.cons` myId `B.append` (n `B.cons` nid)
    genPacks :: Serialize i => [Node i] -> Either String [B.ByteString]
    genPacks [] = return []
    genPacks nodes =
      let fits b     = B.length b < prefixSize
          incPacks   = takeWhile fits . scanl' B.append B.empty . map nodeToArg $ nodes
          argsFitNum = length incPacks
          pack       = last incPacks
      in do when (argsFitNum == 0) $
                throwError "No nodes fit on RETURN_NODES serialization"
            (:) pack <$> genPacks (drop argsFitNum nodes)

serialize size (toBS -> myId) cmd =
    if B.length res + 1 + B.length myId > size
        then throwError "Size exceeded"
        else return $ [cId `B.cons` myId `B.append` res]
  where
    res = commandArgs' cmd
    cId = commandId cmd
    commandArgs' PING                 = B.empty
    commandArgs' PONG                 = B.empty
    commandArgs' (STORE k v)          = toBS k `B.append` toBS v
    commandArgs' (FIND_NODE nid)      = toBS nid
    commandArgs' (FIND_VALUE k)       = toBS k
    commandArgs' (RETURN_VALUE nid v) = toBS nid `B.append` toBS v
    commandArgs' (RETURN_NODES _ _ _) = error "Don't know what to do with this case :("

nodeToArg :: (Serialize i) => Node i -> B.ByteString
nodeToArg node = nid `B.append` C.pack (host ++ " ") `B.append` port
    where nid = toBS . nodeId $ node
          host = peerHost . peer $ node
          port = toBinary . fromIntegral . peerPort . peer $ node
          -- Converts a Word16 into a two character ByteString
          toBinary = B.concat . L.toChunks . toLazyByteString . word16BE
--
---- | Turn a command into a sendable ByteStrings, which fit into specified size.
---- TODO: preserve lazy evaluation of result list.
--serialize :: (Serialize i, Serialize a)
--          => Int -> i -> Command i a -> Either String [B.ByteString]
--serialize size nid command = do
--    let cId  = commandId command
--    let nid' = toBS nid
--    (args, rest) <- commandArgs (size - 1 - B.length nid') command
--    remaining    <- maybe (return []) (serialize size nid) rest
--    return $ (cId `B.cons` nid' `B.append` args) : remaining
