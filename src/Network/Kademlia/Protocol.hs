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
commandId PING               = 0
commandId PONG               = 1
commandId (STORE _ _)        = 2
commandId (FIND_NODE _)      = 3
commandId (RETURN_NODES _ _) = 4
commandId (FIND_VALUE _)     = 5
commandId (RETURN_VALUE _ _) = 6

-- | Turn the command arguments into a ByteString, which fits into specified size.
-- Remaining part of command would be also returned, if any.
commandArgs :: (Serialize i, Serialize a)
            => Int -> Command i a -> Either String (B.ByteString, Maybe (Command i a))
commandArgs size (RETURN_NODES nid kb) = do
    let nid'       = toBS nid
        fits b     = B.length b < size - B.length nid'
        incPacks   = takeWhile fits . scanl' B.append B.empty . map nodeToArg $ kb
        argsFit    = last incPacks
        argsFitNum = length incPacks
        remArgs    = drop argsFitNum kb
        remCmd     = if null remArgs
                        then Nothing
                        else Just $ RETURN_NODES nid remArgs
    when (argsFitNum == 0) $
        throwError "No nodes fit on RETURN_NODES serialization"
    return (nid' `B.append` argsFit, remCmd)

commandArgs size cmd = let res = commandArgs' cmd
                       in  if B.length res > size
                                then throwError "Size exceeded"
                                else return (res, Nothing)
  where
    commandArgs' PING                 = B.empty
    commandArgs' PONG                 = B.empty
    commandArgs' (STORE k v)          = toBS k `B.append` toBS v
    commandArgs' (FIND_NODE nid)      = toBS nid
    commandArgs' (FIND_VALUE k)       = toBS k
    commandArgs' (RETURN_VALUE nid v) = toBS nid `B.append` toBS v
    commandArgs' (RETURN_NODES _ _)   = error "Don't know what to do with this case :("

nodeToArg :: (Serialize i) => Node i -> B.ByteString
nodeToArg node = nid `B.append` C.pack (host ++ " ") `B.append` port
    where nid = toBS . nodeId $ node
          host = peerHost . peer $ node
          port = toBinary . fromIntegral . peerPort . peer $ node
          -- Converts a Word16 into a two character ByteString
          toBinary = B.concat . L.toChunks . toLazyByteString . word16BE

-- | Turn a command into a sendable ByteStrings, which fit into specified size.
-- TODO: preserve lazy evaluation of result list.
serialize :: (Serialize i, Serialize a)
          => Int -> i -> Command i a -> Either String [B.ByteString]
serialize size nid command = do
    let cId  = commandId command
    let nid' = toBS nid
    (args, rest) <- commandArgs (size - 1 - B.length nid') command
    remaining    <- maybe (return []) (serialize size nid) rest
    return $ (cId `B.cons` nid' `B.append` args) : remaining
