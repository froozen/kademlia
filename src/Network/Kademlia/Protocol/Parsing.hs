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

import Network.Kademlia.Types

type Parse = ExceptT String (State B.ByteString)

parse :: (Read a, Id i) => Peer -> B.ByteString -> Either String (Signal i a)
parse peer = evalState (runExceptT $ parseSignal peer)

-- | Parses the parsable parts of a signal
parseSignal :: (Read a, Id i) => Peer -> Parse (Signal i a)
parseSignal peer = do
    cId <- parseCommandId
    id <- parseId
    cmd <- parseCommand cId
    return $ Signal id peer cmd

-- | Parses an Id
parseId :: (Id a) => Parse a
parseId = do
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

-- | Parses a trailing Read instance
parseRead :: (Read a) => Parse a
parseRead = do
    bs <- get
    case readMaybe $ C.unpack bs of
        Nothing -> throwE "Read failed"
        Just r  -> do
            put B.empty
            return r

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

-- | Skips all spaces
skipSpaces :: Parse ()
skipSpaces = do
    bs <- get
    put $ C.dropWhile (==' ') bs

-- | Parses an Int
parseInt :: Parse Int
parseInt = do
    bs <- get
    case C.readInt bs of
        Nothing -> throwE "Failed to parse an Int"
        Just (n, rest) -> do
            put rest
            return n

-- | Parses a peer's info
parsePeer :: Parse Peer
parsePeer = do
    skipSpaces
    host <- parseSplit ' '
    skipSpaces
    port <- parseInt
    return $ Peer (C.unpack host) (fromIntegral port)

-- | Parses a trailing k-bucket
parseKBucket :: Parse KBucket
parseKBucket = do
    peer <- parsePeer
    catchE
        (liftM (peer:) parseKBucket)
        (\_ -> return [peer])

-- | Parses the rest of a command corresponding to an id
parseCommand :: (Read a, Id i) => Int -> Parse (Command i a)
parseCommand 0 = return PING
parseCommand 1 = liftM2 STORE parseId parseRead
parseCommand 2 = FIND_NODE `liftM` parseId
parseCommand 3 = RETURN_NODES `liftM` parseKBucket
parseCommand 4 = FIND_VALUE `liftM` parseId
parseCommand 5 = RETURN_VALUE `liftM` parseRead
parseCommand _ = throwE "Invalid id"
