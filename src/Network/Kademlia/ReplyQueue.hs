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
    , ReplyQueue(..)
    , emptyReplyQueue
    , emptyReplyQueueL
    , register
    , dispatch
    , flush
    ) where

import           Control.Concurrent     (Chan, ThreadId, forkIO, killThread, newChan,
                                         writeChan)
import           Control.Concurrent.STM (TVar, atomically, newTVar, readTVar, writeTVar)
import           Control.Monad          (forM_)
import           Data.List              (delete, find)

import           Network.Kademlia.Types (Command (..), Node (..), Signal (..))
import           Network.Kademlia.Utils (threadDelay)

-- | The different types a replied signal could possibly have.
--
--   Note that these are only those Command types, which are replies to some
--   sort of request. Therefore, most Command types aren't contained in this
--   type.
data ReplyType i = R_PONG
                 | R_RETURN_VALUE i
                 | R_RETURN_NODES i
                   deriving (Eq, Show)

-- | The representation of registered replies
data ReplyRegistration i = RR {
      replyTypes  :: [ReplyType i]
    , replyOrigin :: i
    } deriving (Eq, Show)

-- | Convert a Signal into its ReplyRegistration representation
toRegistration :: Reply i a -> Maybe (ReplyRegistration i)
toRegistration Closed        = Nothing
toRegistration (Timeout reg) = Just reg
toRegistration (Answer sig)  = case rType . command $ sig of
            Nothing -> Nothing
            Just rt -> Just (RR [rt] origin)
    where origin = nodeId $ source sig

          rType :: Command i a -> Maybe (ReplyType i)
          rType  PONG                  = Just  R_PONG
          rType (RETURN_VALUE nid _)   = Just (R_RETURN_VALUE nid)
          rType (RETURN_NODES _ nid _) = Just (R_RETURN_NODES nid)
          rType _                      = Nothing

-- | Compare wether two ReplyRegistrations match
matchRegistrations :: (Eq i) => ReplyRegistration i -> ReplyRegistration i -> Bool
matchRegistrations (RR rtsA idA) (RR rtsB idB) =
    idA == idB && (all (`elem` rtsA) rtsB || all (`elem` rtsB) rtsA)

-- | The actual type of a replay
data Reply i a = Answer (Signal i a)
               | Timeout (ReplyRegistration i)
               | Closed
                 deriving (Eq, Show)

-- | The actual type representing a ReplyQueue
data ReplyQueue i a = RQ {
      queue       :: (TVar [(ReplyRegistration i, Chan (Reply i a), ThreadId)])
    , timeoutChan :: Chan (Reply i a)
    , defaultChan :: Chan (Reply i a)
    , logInfo     :: String -> IO ()
    , logError    :: String -> IO ()
    }


-- | Create a new ReplyQueue
emptyReplyQueue :: IO (ReplyQueue i a)
emptyReplyQueue = emptyReplyQueueL (const $ pure ()) (const $ pure ())

-- | Create a new ReplyQueue with loggers
emptyReplyQueueL :: (String -> IO ()) -> (String -> IO ()) -> IO (ReplyQueue i a)
emptyReplyQueueL logInfo logError =
    RQ <$> (atomically . newTVar $ []) <*> newChan <*> newChan <*> pure logInfo <*>
    pure logError

-- | Register a channel as handler for a reply
register
    :: ReplyRegistration i
    -> ReplyQueue i a
    -> Chan (Reply i a)
    -> IO ()
register reg rq chan = do
    tId <- timeoutThread reg rq
    atomically $ do
        rQueue <- readTVar $ queue rq
        writeTVar (queue rq) $ rQueue ++ [(reg, chan, tId)]

timeoutThread :: ReplyRegistration i -> ReplyQueue i a -> IO ThreadId
timeoutThread reg rq = forkIO $ do
    -- Wait 5 seconds
    threadDelay 5

    -- Remove the ReplyRegistration from the ReplyQueue
    -- TODO: What should be here?
    -- myTId <- myThreadId

    -- Send Timeout signal
    writeChan (timeoutChan rq) . Timeout $ reg

-- | Dispatch a reply over a registered handler. If there is no handler,
--   dispatch it to the default one.
dispatch :: (Show i, Eq i) => Reply i a -> ReplyQueue i a -> IO ()
dispatch reply rq = do
    -- Try to find a registration matching the reply
    result <- atomically $ do
        rQueue <- readTVar . queue $ rq
        case toRegistration reply of
            Just repReg -> case find (matches repReg) rQueue of
                Just registration -> do
                    -- Remove registration from queue
                    writeTVar (queue rq) $ delete registration rQueue
                    return . Just $ registration

                Nothing -> return Nothing
            Nothing -> return Nothing
    case result of
        Just (reg, chan, tId) -> do
            logInfo rq (" -- dispatch reply " ++ show reply ++ ": in queue, " ++ show reg)
            -- Kill the timeout thread
            killThread tId

            -- Send the reply
            writeChan chan reply

        -- Send the reply over the default channel
        Nothing -> do
          logInfo rq (" -- dispatch reply " ++ show reply ++ ": not in queue")
          writeChan (defaultChan rq) reply

    where matches regA (regB, _, _) = matchRegistrations regA regB

-- | Send Closed signal to all handlers and empty ReplyQueue
flush :: ReplyQueue i a -> IO ()
flush rq = do
    rQueue <- atomically $ do
        rQueue <- readTVar . queue $ rq
        writeTVar (queue rq) []
        return rQueue

    forM_ rQueue $ \(_, chan, tId) -> do
        killThread tId
        writeChan chan Closed
