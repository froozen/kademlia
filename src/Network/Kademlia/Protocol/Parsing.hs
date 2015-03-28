{-|
Module      : Network.Kademlia.Protocol.Parsing
Description : Implementation of the protocol parsing

Netowrk.Kademlia.Protocol.Parsing implements the actual protocol parsing.

It made sense to split it off Network.Kademlia.Protocol as it made both cleaner
and more readable.
-}

module Network.Kademlia.Protocol.Parsing where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Control.Monad (liftM, liftM2)
import Control.Monad.State
import Control.Monad.Trans.Except
import Text.Read (readMaybe)
import Data.Word (Word8, Word16)
import Data.Bits (shiftL)

import Network.Kademlia.Types

type Parse = ExceptT String (State B.ByteString)

parse :: (Serialize i, Serialize a) => Peer -> B.ByteString -> Either String (Signal i a)
parse peer = evalState (runExceptT $ parseSignal peer)

-- | Parses the parsable parts of a signal
parseSignal :: (Serialize i, Serialize a) => Peer -> Parse (Signal i a)
parseSignal peer = do
    cId <- parseCommandId
    id <- parseSerialize
    cmd <- parseCommand cId
    return $ Signal id peer cmd

-- | Parses a Serialize
parseSerialize :: (Serialize a) => Parse a
parseSerialize = do
    bs <- get
    case fromBS bs of
        Left err -> throwE err
        Right (id, rest) -> do
            put rest
            return id

-- | Parses a CommandId
parseCommandId :: Parse Int
parseCommandId = do
    bs <- get
    case B.uncons bs of
        Nothing         -> throwE "uncons returned Nothing"
        Just (id, rest) -> do
            put rest
            return $ fromIntegral id

-- | Splits after a certain character
parseSplit :: Char -> Parse B.ByteString
parseSplit c = do
    bs <- get
    if B.null bs
        then throwE "ByteString empty"
        else do
            let (result, rest) = C.span (/=c) bs
            put rest
            return result

-- | Skips one character
skipCharacter :: Parse ()
skipCharacter = do
    bs <- get
    if B.null bs
        then throwE "ByteString empty"
        else put $ B.drop 1 bs

-- | Parses an Int
parseInt :: Parse Int
parseInt = do
    bs <- get
    case C.readInt bs of
        Nothing -> throwE "Failed to parse an Int"
        Just (n, rest) -> do
            put rest
            return n

-- | Parses two Word8s from a ByteString into one Word16
parseWord16 :: Parse Word16
parseWord16 = do
    bs <- get
    if B.length bs < 2
        then throwE "ByteString to short"
        else do
            let (words, rest) = B.splitAt 2 bs
            put rest
            return . joinWords . B.unpack $ words
    where
        joinWords [a, b] = (toWord16 a `shiftL` 8) + toWord16 b

        toWord16 :: Word8 -> Word16
        toWord16 = fromIntegral

-- | Parses a peer's info
parsePeer :: Parse Peer
parsePeer = do
    host <- parseSplit ' '
    skipCharacter
    port <- parseWord16
    return $ Peer (C.unpack host) (fromIntegral port)

-- | Parses a trailing k-bucket
parseKBucket :: Parse KBucket
parseKBucket = do
    peer <- parsePeer
    catchE
        (liftM (peer:) parseKBucket)
        (\_ -> return [peer])

-- | Parses the rest of a command corresponding to an id
parseCommand :: (Serialize i, Serialize a) => Int -> Parse (Command i a)
parseCommand 0 = return PING
parseCommand 1 = liftM2 STORE parseSerialize parseSerialize
parseCommand 2 = FIND_NODE `liftM` parseSerialize
parseCommand 3 = RETURN_NODES `liftM` parseKBucket
parseCommand 4 = FIND_VALUE `liftM` parseSerialize
parseCommand 5 = RETURN_VALUE `liftM` parseSerialize
parseCommand _ = throwE "Invalid id"
