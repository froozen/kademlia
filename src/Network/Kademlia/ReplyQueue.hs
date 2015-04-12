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

import Control.Concurrent.STM
import Control.Concurrent.Chan
import Control.Monad (liftM, forM_)
import Control.Monad.Trans.Maybe
import Data.List (lookup, deleteBy)

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

-- | The representation of a registered Reply
data ReplyRegistration i = ReplyRegistration {
      replyType   :: ReplyType i
    , replyOrigin :: i
    } deriving (Eq)

-- | Convert a Signal into its ReplyRegistration representation
toRegistration :: Signal i a -> Maybe (ReplyRegistration i)
toRegistration sig = case rType . command $ sig of
                        Nothing -> Nothing
                        Just rt -> Just (ReplyRegistration rt origin)
    where origin = nodeId . source $ sig

          rType :: Command i a -> Maybe (ReplyType i)
          rType  PONG               = Just  R_PONG
          rType (RETURN_VALUE id _) = Just (R_RETURN_VALUE id)
          rType (RETURN_NODES id _) = Just (R_RETURN_NODES id)
          rType _ = Nothing

-- | The actual type of a replay
data Reply i a = Answer (Signal i a)
               | Closed
                 deriving (Eq, Show)

-- | The actual type representing a ReplyQueue
newtype ReplyQueue i a = RQ (TVar [(ReplyRegistration i, Chan (Reply i a))])

-- | Create an empty ReplyQueue
emptyReplyQueue :: IO (ReplyQueue i a)
emptyReplyQueue = atomically . liftM RQ $ newTVar []

-- | Register a channel as handler for a reply
register :: ReplyRegistration i -> ReplyQueue i a -> Chan (Reply i a)
         -> IO ()
register reg (RQ rq) chan = atomically $ do
    queue <- readTVar rq
    writeTVar rq $ queue ++ [(reg, chan)]

-- | Try to send a received Signal over the registered handler channel and
--   return wether it succeeded
dispatch :: (Eq i) => Signal i a -> ReplyQueue i a -> IO Bool
dispatch sig (RQ rq) = do
    queue <- atomically . readTVar $ rq
    case toRegistration sig of
        Nothing  -> return False
        Just reg -> case lookup reg queue of
            Just chan -> do
                -- Send the signal
                writeChan chan $ Answer sig

                -- Remove registration from queue
                atomically . writeTVar rq $
                    deleteBy (\_ a -> fst a == reg) undefined queue
                return True
            Nothing -> return False

-- | Send Closed signal to all handlers and empty ReplyQueue
flush :: ReplyQueue i a -> IO ()
flush (RQ rq) = do
    queue <- atomically . readTVar $ rq
    forM_ queue $ \(_, chan) -> writeChan chan Closed
    atomically . writeTVar rq $ []
