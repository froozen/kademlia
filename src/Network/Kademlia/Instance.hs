{-|
Module      : Network.Kademlia.Instance
Description : Implementation of the KademliaInstance type

"Network.Kademlia.Instance" implements the KademliaInstance type, as well
as all the things that need to happen in the background to get a working
Kademlia instance.
-}

module Network.Kademlia.Instance
    ( KademliaInstance(..)
    , KademliaState(..)
    , start
    , newInstance
    , insertNode
    , deleteNode
    , lookupNode
    ) where

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.STM
import Control.Monad (void, forever, when, join, forM_, forever)
import Control.Monad.Trans
import Control.Monad.Trans.State hiding (state)
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class (liftIO)
import Control.Applicative ((<$>), (<*>))
import System.IO.Error (catchIOError)
import qualified Data.Map as M
import Data.Maybe (catMaybes, isJust)
import Data.Function (on)
import Data.Map (toList)

import Network.Kademlia.Networking
import qualified Network.Kademlia.Tree as T
import Network.Kademlia.Types
import Network.Kademlia.ReplyQueue

-- | The handle of a running Kademlia Node
data KademliaInstance i a = KI {
      handle :: KademliaHandle i a
    , state :: KademliaState i a
    }

-- | Representation of the data the KademliaProcess carries
data KademliaState i a = KS {
      sTree   :: TVar (T.NodeTree i)
    , values :: TVar (M.Map i a)
    }

-- | Create a new KademliaInstance from an Id and a KademliaHandle
newInstance :: (Serialize i) =>
               i -> KademliaHandle i a -> IO (KademliaInstance i a)
newInstance id handle = do
    tree <- atomically . newTVar . T.create $ id
    values <- atomically . newTVar $ M.empty
    return . KI handle . KS tree $ values

insertNode :: (Serialize i, Ord i) => KademliaInstance i a -> Node i -> IO ()
insertNode (KI _ (KS sTree _)) node = atomically $ do
    tree <- readTVar sTree
    writeTVar sTree . T.insert tree $ node

deleteNode :: (Serialize i, Ord i) => KademliaInstance i a -> i -> IO ()
deleteNode (KI _ (KS sTree _)) id = atomically $ do
    tree <- readTVar sTree
    writeTVar sTree . T.delete tree $ id

lookupNode :: (Serialize i, Ord i) => KademliaInstance i a -> i -> IO (Maybe (Node i))
lookupNode (KI _ (KS sTree _)) id = atomically $ do
    tree <- readTVar sTree
    return . T.lookup tree $ id

insertValue :: (Ord i) => i -> a -> KademliaInstance i a -> IO ()
insertValue key value (KI _ (KS _ values)) = atomically $ do
    vals <- readTVar values
    writeTVar values $ M.insert key value vals

lookupValue :: (Ord i) => i -> KademliaInstance i a -> IO (Maybe a)
lookupValue key (KI _ (KS _ values)) = atomically $ do
    vals <- readTVar values
    return . M.lookup key $ vals

-- | Start the background process for a KademliaInstance
start :: (Serialize i, Ord i, Serialize a, Eq i, Eq a) =>
         KademliaInstance i a -> ReplyQueue i a -> IO ()
start inst rq = do
        startRecvProcess . handle $ inst
        let rChan = timeoutChan rq
            dChan = defaultChan rq
        receivingId <- forkIO . receivingProcess inst rq $ rChan
        pingId <- forkIO . pingProcess inst $ dChan
        spreadId <- forkIO . spreadValueProcess $ inst
        void . forkIO $ backgroundProcess inst dChan [pingId, spreadId, receivingId]

-- | The central process all Replys go trough
receivingProcess :: (Serialize i, Serialize a, Eq i, Ord i) =>
    KademliaInstance i a -> ReplyQueue i a -> Chan (Reply i a) -> IO ()
receivingProcess inst rq chan = forever $ do
    reply <- readChan chan

    case reply of
        -- Delete a node that timed out
        Timeout registration -> deleteNode inst . replyOrigin $ registration

        -- Store values in newly encountered nodes that you are the closest to
        Answer (Signal node _) -> do
            let originId = nodeId node
            tree <- retrieve sTree

            -- This node is not yet known
            when (not . isJust . T.lookup tree $ originId) $ do
                let closestKnown = T.findClosest tree originId 1
                    ownId = T.extractId tree
                    self = node { nodeId = ownId }
                    bucket = self:closestKnown
                    -- Find out closest known node
                    closestId = nodeId . head . sortByDistanceTo bucket $ originId

                -- This node can be assumed to be closest to the new node
                when (ownId == closestId) $ do
                    storedValues <- toList <$> retrieve values
                    let h = handle inst
                        p = peer node
                    -- Store all stored values in the new node
                    forM_ storedValues (send h p . uncurry STORE)

        _ -> return ()

    dispatch reply rq
    where retrieve f = atomically . readTVar . f . state $ inst


-- | The actual process running in the background
backgroundProcess :: (Serialize i, Ord i, Serialize a, Eq i, Eq a) =>
    KademliaInstance i a -> Chan (Reply i a) -> [ThreadId] -> IO ()
backgroundProcess inst chan threadIds = do
    reply <- liftIO . readChan $ chan

    case reply of
        Answer sig -> do
            let node = source sig

            -- Handle the signal
            handleCommand (command sig) (peer node) inst

            -- Insert the node into the tree, if it's allready known, it will
            -- be refreshed
            insertNode inst node

            backgroundProcess inst chan threadIds

        -- Kill pingProcess and stop on Closed
        Closed -> mapM_ killThread threadIds

        _ -> return ()

-- | Ping all known nodes every five minutes to make sure they are still present
pingProcess :: (Serialize i, Serialize a, Eq i) => KademliaInstance i a
            -> Chan (Reply i a) -> IO ()
pingProcess (KI h (KS sTree _)) chan = forever $ do
    threadDelay fiveMinutes

    tree <- atomically . readTVar $ sTree
    forM_ (T.toList tree) $ \node -> do
        -- Send PING and expect a PONG
        send h (peer node) PING
        expect h (RR [R_PONG] (nodeId node)) $ chan

    where fiveMinutes = 300000000

-- | Store all values stored in the node in the 7 closest known nodes every hour
spreadValueProcess :: (Serialize i, Serialize a, Eq i) => KademliaInstance i a
                   -> IO ()
spreadValueProcess (KI h (KS sTree sValues)) = forever $ do
    threadDelay hour

    values <- atomically . readTVar $ sValues
    tree <- atomically . readTVar $ sTree

    mapMWithKey (sendRequests tree) $ values

    where hour = 60 * 60 * 1000000
          sendRequests tree key val = do
            let closest = T.findClosest tree key 7
            forM_ closest $ \node -> send h (peer node) (STORE key val)

          mapMWithKey :: (k -> v -> IO a) -> M.Map k v -> IO [a]
          mapMWithKey f m = sequence . map snd . M.toList . M.mapWithKey f $ m

-- | Handles the differendt Kademlia Commands appropriately
handleCommand :: (Serialize i, Eq i, Ord i, Serialize a) =>
    Command i a -> Peer -> KademliaInstance i a -> IO ()
-- Simply answer a PING with a PONG
handleCommand PING peer inst = send (handle inst) peer PONG
-- Return a KBucket with the closest Nodes
handleCommand (FIND_NODE id) peer inst = returnNodes peer id inst
-- Insert the value into the values Map
handleCommand (STORE key value) _ inst = insertValue key value inst
-- Return the value, if known, or the closest other known Nodes
handleCommand (FIND_VALUE key) peer inst = do
    result <- lookupValue key inst
    case result of
        Just value -> liftIO $ send (handle inst) peer $ RETURN_VALUE key value
        Nothing    -> returnNodes peer key inst
handleCommand _ _ _ = return ()

-- | Return a KBucket with the closest Nodes to a supplied Id
returnNodes :: (Serialize i, Eq i, Ord i, Serialize a) =>
    Peer -> i -> KademliaInstance i a -> IO ()
returnNodes peer id (KI h (KS sTree _)) = do
    tree <- atomically . readTVar $ sTree
    let nodes = T.findClosest tree id 7
    liftIO $ send h peer (RETURN_NODES id nodes)
