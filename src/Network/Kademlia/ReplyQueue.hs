{-|
Module      : Network.Kademlia.ReplyQueue
Description : A queue allowing to register handlers for expected replies

Network.Kademlia.ReplyQueue implements a Queue designed for registering
handlers for expected replies.

The handlers are represented by unbound channels from Control.Concurrency.Chan.
-}

module Network.Kademlia.ReplyQueue
    ( ReplyType(..)
    , ReplyRegistration(..)
    , Reply(..)
    , ReplyQueue
    , emptyReplyQueue
    , register
    , dispatch
    , flush
    ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Chan
import Control.Monad (liftM, forM_)
import Control.Monad.Trans.Maybe
import Data.List (find, delete)

import Network.Kademlia.Types

-- | The different types a replied signal could possibly have.
--
--   Note that these are only those Command types, which are replies to some
--   sort of request. Therefore, most Command types aren't contained in this
--   type.
data ReplyType i = R_PONG
                 | R_RETURN_VALUE i
                 | R_RETURN_NODES i
                   deriving (Eq)

-- | The representation of registered replies
data ReplyRegistration i = RR {
      replyTypes  :: [ReplyType i]
    , replyOrigin :: i
    } deriving (Eq)

-- | Convert a Signal into its ReplyRegistration representation
toRegistration :: Signal i a -> Maybe (ReplyRegistration i)
toRegistration sig = case rType . command $ sig of
                        Nothing -> Nothing
                        Just rt -> Just (RR [rt] origin)
    where origin = nodeId . source $ sig

          rType :: Command i a -> Maybe (ReplyType i)
          rType  PONG               = Just  R_PONG
          rType (RETURN_VALUE id _) = Just (R_RETURN_VALUE id)
          rType (RETURN_NODES id _) = Just (R_RETURN_NODES id)
          rType _ = Nothing

-- | Compare wether two ReplyRegistrations match
matchRegistrations :: (Eq i) => ReplyRegistration i -> ReplyRegistration i -> Bool
matchRegistrations (RR rtsA idA) (RR rtsB idB) =
    idA == idB && (all (`elem` rtsA) rtsB || all (`elem` rtsB) rtsA)

-- | The actual type of a replay
data Reply i a = Answer (Signal i a)
               | Timeout (ReplyRegistration i)
               | Closed
                 deriving (Eq)

-- | The actual type representing a ReplyQueue
newtype ReplyQueue i a = RQ (TVar [(ReplyRegistration i, Chan (Reply i a), ThreadId)])

-- | Create an empty ReplyQueue
emptyReplyQueue :: IO (ReplyQueue i a)
emptyReplyQueue = atomically . liftM RQ $ newTVar []

-- | Register a channel as handler for a reply
register :: (Eq i) => ReplyRegistration i -> ReplyQueue i a -> Chan (Reply i a)
         -> IO ()
register reg (RQ rq) chan = do
    tId <- timeoutThread chan reg (RQ rq)
    atomically $ do
        queue <- readTVar $ rq
        writeTVar rq $ queue ++ [(reg, chan, tId)]

timeoutThread :: (Eq i) => Chan (Reply i a) -> ReplyRegistration i
              -> ReplyQueue i a -> IO ThreadId
timeoutThread chan reg (RQ rq) = forkIO $ do
    -- Wait 5 seconds
    threadDelay 5000000

    -- Remove the ReplyRegistration from the ReplyQueue
    myTId <- myThreadId
    atomically $ do
        queue <- readTVar $ rq
        case find (\(_, _, tId) -> tId == myTId) queue of
            Just rqElem -> writeTVar rq $ delete rqElem queue
            _ -> return ()

    -- Send Timeout signal
    writeChan chan . Timeout $ reg

-- | Try to send a received Signal over the registered handler channel and
--   return wether it succeeded
dispatch :: (Eq i) => Signal i a -> ReplyQueue i a -> IO Bool
dispatch sig (RQ rq) = do
    result <- atomically $ do
        queue <- readTVar $ rq
        case toRegistration sig of
            Just regA -> case find (matches regA) queue of
                Just reg -> do
                    -- Remove registration from queue
                    writeTVar rq $ delete reg queue
                    return . Just $ reg

                Nothing -> return Nothing
            Nothing  -> return Nothing

    case result of
        Just (_, chan, tId) -> do
            -- Kill the timeout thread
            killThread tId

            -- Send the signal
            writeChan chan $ Answer sig
            return True
        _ -> return False

    where matches regA (regB, _, _) = matchRegistrations regA regB

-- | Send Closed signal to all handlers and empty ReplyQueue
flush :: ReplyQueue i a -> IO ()
flush (RQ rq) = do
    queue <- atomically $ do
        queue <- readTVar $ rq
        writeTVar rq $ []
        return queue

    forM_ queue $ \(_, chan, tId) -> do
        killThread tId
        writeChan chan Closed
