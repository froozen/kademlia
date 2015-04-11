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
    , ReplyQueue
    , emptyReplyQueue
    , register
    , dispatch
    ) where

import Control.Concurrent.STM
import Control.Monad (liftM)
import Control.Monad.Trans.Maybe
import Data.List (lookup, deleteBy)

import Network.Kademlia.Types

-- | The different types a reply could possibly have.
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

-- | The actual type representing a ReplyQueue
newtype ReplyQueue i a = RQ (TVar [(ReplyRegistration i, TChan (Signal i a))])

-- | Create an empty ReplyQueue
emptyReplyQueue :: STM (ReplyQueue i a)
emptyReplyQueue = liftM RQ $ newTVar []

-- | Register a channel as handler for a reply
register :: ReplyRegistration i -> ReplyQueue i a -> TChan (Signal i a)
         -> STM ()
register reg (RQ rq) chan = do
    queue <- readTVar rq
    writeTVar rq $ queue ++ [(reg, chan)]

-- | Try to send a received Signal over the registered handler channel and
--   return wether it succeeded
dispatch :: (Eq i) => Signal i a -> ReplyQueue i a -> STM Bool
dispatch sig (RQ rq) = do
    queue <- readTVar  rq
    case toRegistration sig of
        Nothing  -> return False
        Just reg -> case lookup reg queue of
            Just chan -> do
                -- Send the signal
                writeTChan chan sig

                -- Remove registration from queue
                writeTVar rq $
                    deleteBy (\_ a -> fst a == reg) undefined queue
                return True
            Nothing -> return False
