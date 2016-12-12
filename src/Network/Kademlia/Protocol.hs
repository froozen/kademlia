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

import           Network.Kademlia.Protocol.Parsing
import           Network.Kademlia.Types

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
commandArgs size (RETURN_NODES id kb) = do
    let id'        = toBS id
        fits b     = B.length b < size - B.length id'
        incPacks   = takeWhile fits . scanl' B.append B.empty . map nodeToArg $ kb
        argsFit    = last incPacks
        argsFitNum = length incPacks
        remArgs    = drop argsFitNum kb
        remCmd     = if null remArgs
                        then Nothing
                        else Just $ RETURN_NODES id remArgs
    when (argsFitNum == 0) $
        throwError "No nodes fit on RETURN_NODES serialization"
    return (id' `B.append` argsFit, remCmd)

commandArgs size cmd = let res = commandArgs' cmd
                       in  if B.length res > size
                                then throwError "Size exceeded"
                                else return (res, Nothing)
  where
    commandArgs' PING                 = B.empty
    commandArgs' PONG                 = B.empty
    commandArgs' (STORE k v)          = toBS k `B.append` toBS v
    commandArgs' (FIND_NODE id)       = toBS id
    commandArgs' (FIND_VALUE k)       = toBS k
    commandArgs' (RETURN_VALUE id v)  = toBS id `B.append` toBS v

nodeToArg :: (Serialize i) => Node i -> B.ByteString
nodeToArg node = id `B.append` C.pack (host ++ " ") `B.append` port
    where id = toBS . nodeId $ node
          host = peerHost . peer $ node
          port = toBinary . fromIntegral . peerPort . peer $ node
          -- Converts a Word16 into a two character ByteString
          toBinary = B.concat . L.toChunks . toLazyByteString . word16BE

-- | Turn a command into a sendable ByteStrings, which fit into specified size.
-- TODO: preserve lazy evaluation of result list.
serialize :: (Serialize i, Serialize a)
          => Int -> i -> Command i a -> Either String [B.ByteString]
serialize size id command = do
    let cId = commandId command
        id' = toBS id
    (args, rem) <- commandArgs (size - 1 - B.length id') command
    remaining   <- maybe (return []) (serialize size id) rem
    return $ (cId `B.cons` id' `B.append` args) : remaining
