{-|
Module      : Network.Kademlia.Protocol.Parsing
Description : Implementation of the protocol parsing

Network.Kademlia.Protocol.Parsing implements the actual protocol parsing.

It made sense to split it off Network.Kademlia.Protocol as it made both cleaner
and more readable.
-}

module Network.Kademlia.Protocol.Parsing
       ( parse
       , parseCommandId
       , parseSerialize
       , parseSignal
       , skipCharacter
       , parseInt
       , parseWord16
       ) where

import           Control.Monad              (liftM, liftM2)
import           Control.Monad.State        (State, evalState, get, put)
import           Control.Monad.Trans        (lift)
import           Control.Monad.Trans.Except (ExceptT, catchE, runExceptT, throwE)
import           Data.Bits                  (shiftL)
import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as C
import           Data.Word                  (Word16, Word8)

import           Network.Kademlia.Types     (Command (..), Node (..), Peer (..),
                                             Serialize (..), Signal (..))

type Parse = ExceptT String (State B.ByteString)

-- | Parse a signal from a ByteString
--
--   (This needs to be supplied a Peer, to be able to create a complete Signal)
parse :: (Serialize i, Serialize a) => Peer -> B.ByteString -> Either String (Signal i a)
parse peer = evalState (runExceptT $ parseSignal peer)

-- | Parses the parsable parts of a signal
parseSignal :: (Serialize i, Serialize a) => Peer -> Parse (Signal i a)
parseSignal peer = do
    cId <- parseCommandId
    nid <- parseSerialize
    cmd <- parseCommand cId
    let node = Node peer nid
    return $ Signal node cmd

-- | Parses a Serialize
parseSerialize :: (Serialize a) => Parse a
parseSerialize = do
    bs <- lift get
    case fromBS bs of
        Left err -> throwE err
        Right (nid, rest) -> do
            lift . put $ rest
            return nid

-- | Parses a CommandId
parseCommandId :: Parse Int
parseCommandId = do
    bs <- lift get
    case B.uncons bs of
        Nothing         -> throwE "uncons returned Nothing"
        Just (nid, rest) -> do
            lift . put $ rest
            return $ fromIntegral nid

-- | Splits after a certain character
parseSplit :: Char -> Parse B.ByteString
parseSplit c = do
    bs <- lift get
    if B.null bs
        then throwE "ByteString empty"
        else do
            let (result, rest) = C.span (/=c) bs
            lift . put $ rest
            return result

-- | Skips one character
skipCharacter :: Parse ()
skipCharacter = do
    bs <- lift get
    if B.null bs
        then throwE "ByteString empty"
        else lift . put $ B.drop 1 bs

-- | Parses an Int
parseInt :: Parse Int
parseInt = do
    bs <- lift get
    case C.readInt bs of
        Nothing -> throwE "Failed to parse an Int"
        Just (n, rest) -> do
            lift . put $ rest
            return n

-- | Parses two Word8s from a ByteString into one Word16
parseWord16 :: Parse Word16
parseWord16 = do
    bs <- lift  get
    if B.length bs < 2
        then throwE "ByteString to short"
        else do
            let (byteWords, rest) = B.splitAt 2 bs
            lift . put $ rest
            return . joinWords . B.unpack $ byteWords
    where
        joinWords [a, b] = (toWord16 a `shiftL` 8) + toWord16 b
        joinWords _      = error "Panic! Error in join words"

        toWord16 :: Word8 -> Word16
        toWord16 = fromIntegral

-- | Parses a Node's info
parseNode :: (Serialize i) => Parse (Node i)
parseNode = do
    nid <- parseSerialize
    host <- parseSplit ' '
    skipCharacter
    port <- parseWord16
    let peer = Peer (C.unpack host) (fromIntegral port)
    return $ Node peer nid

-- | Parses a trailing k-bucket
parseKBucket :: (Serialize i) => Parse [Node i]
parseKBucket = liftM2 (:) parseNode parseKBucket
                   `catchE` \_ -> return []

-- | Parses the rest of a command corresponding to an id
parseCommand :: (Serialize i, Serialize a) => Int -> Parse (Command i a)
parseCommand 0 = return PING
parseCommand 1 = return PONG
parseCommand 2 = liftM2 STORE parseSerialize parseSerialize
parseCommand 3 = FIND_NODE `liftM` parseSerialize
parseCommand 4 = liftM2 RETURN_NODES  parseSerialize parseKBucket
parseCommand 5 = FIND_VALUE `liftM` parseSerialize
parseCommand 6 = liftM2 RETURN_VALUE parseSerialize parseSerialize
parseCommand _ = throwE "Invalid id"
