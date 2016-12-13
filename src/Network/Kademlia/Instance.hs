{-|
Module      : Network.Kademlia.Instance
Description : Implementation of the KademliaInstance type

"Network.Kademlia.Instance" implements the KademliaInstance type, as well
as all the things that need to happen in the background to get a working
Kademlia instance.
-}

module Network.Kademlia.Instance
    ( KademliaInstance (..)
    , KademliaState    (..)
    , defaultConfig
    , start
    , newInstance
    , insertNode
    , lookupNode
    , dumpPeers
    , banNode
    , isNodeBanned
    ) where

import           Control.Concurrent          (ThreadId, forkIO, killThread, myThreadId)
import           Control.Concurrent.Chan     (Chan, readChan)
import           Control.Concurrent.STM      (TVar, atomically, modifyTVar, newTVar,
                                              readTVar, writeTVar)
import           Control.Exception           (catch)
import           Control.Monad               (forM_, forever, forever, void, when)
import           Control.Monad               (unless)
import           Control.Monad.Extra         (unlessM)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Trans         ()
import           Control.Monad.Trans.Reader  ()
import           Control.Monad.Trans.State   ()
import           Data.Map                    (Map, toList)
import qualified Data.Map                    as M hiding (Map)
import           Data.Maybe                  (fromJust, isJust)
import           System.Random               (newStdGen)

import           Network.Kademlia.Config     (KademliaConfig (..), defaultConfig)
import           Network.Kademlia.Networking (KademliaHandle (..), expect, logError',
                                              send, startRecvProcess)
import           Network.Kademlia.ReplyQueue (Reply (..), ReplyQueue (timeoutChan),
                                              ReplyRegistration (..), ReplyType (..),
                                              defaultChan, dispatch)
import qualified Network.Kademlia.Tree       as T
import           Network.Kademlia.Types      (Command (..), Node (..), Peer (..),
                                              Serialize (..), Signal (..),
                                              sortByDistanceTo)
import           Network.Kademlia.Utils      (threadDelay)

-- | The handle of a running Kademlia Node
data KademliaInstance i a = KI {
      handle            :: KademliaHandle i a
    , state             :: KademliaState i a
    , expirationThreads :: TVar (Map i ThreadId)
    , config            :: KademliaConfig
    }

-- | Representation of the data the KademliaProcess carries
data KademliaState i a = KS {
      sTree  :: TVar (T.NodeTree i)
    , banned :: TVar (Map i (IO Bool))  -- list of banned node;
                                        -- map value == False if ban expired
    , values :: Maybe (TVar (Map i a))
    }

-- | Create a new KademliaInstance from an Id and a KademliaHandle
newInstance :: (Serialize i) =>
               i -> KademliaConfig -> KademliaHandle i a -> IO (KademliaInstance i a)
newInstance nid cfg handle = do
    tree <- atomically . newTVar . T.create $ nid
    banned <- atomically . newTVar $ M.empty
    values <- if storeValues cfg then Just <$> (atomically . newTVar $ M.empty) else pure Nothing
    threads <- atomically . newTVar $ M.empty
    return $ KI handle (KS tree banned values) threads cfg

-- | Insert a Node into the NodeTree
insertNode :: (Serialize i, Ord i) => KademliaInstance i a -> Node i -> IO ()
insertNode (KI _ (KS sTree banned _) _ _) node = do
    ban <- atomically $ readTVar banned
    let isBanned = M.member (nodeId node) ban
    unless isBanned $ atomically $ do
        tree <- readTVar sTree
        writeTVar sTree . T.insert tree $ node

-- | Signal a Node's timeout and retur wether it should be repinged
timeoutNode :: (Serialize i, Ord i) => KademliaInstance i a -> i -> IO Bool
timeoutNode (KI _ (KS sTree _ _) _ _) nid = atomically $ do
    tree <- readTVar sTree
    let (newTree, pingAgain) = T.handleTimeout tree nid
    writeTVar sTree newTree
    return pingAgain

-- | Lookup a Node in the NodeTree
lookupNode :: (Serialize i, Ord i) => KademliaInstance i a -> i -> IO (Maybe (Node i))
lookupNode (KI _ (KS sTree _ _) _ _) nid = atomically $ do
    tree <- readTVar sTree
    return . T.lookup tree $ nid

-- | Return all the Nodes an Instance has encountered so far
dumpPeers :: KademliaInstance i a -> IO [Node i]
dumpPeers (KI _ (KS sTree _ _) _ _) = atomically $ do
    tree <- readTVar sTree
    return . T.toList $ tree

-- | Insert a value into the store
insertValue :: (Ord i) => i -> a -> KademliaInstance i a -> IO ()
insertValue _ _ (KI _ (KS _ _ Nothing) _ _)             = return ()
insertValue key value (KI _ (KS _ _ (Just values)) _ _) = atomically $ do
    vals <- readTVar values
    writeTVar values $ M.insert key value vals

-- | Delete a value from the store
deleteValue :: (Ord i) => i -> KademliaInstance i a -> IO ()
deleteValue _ (KI _ (KS _ _ Nothing) _ _)         = return ()
deleteValue key (KI _ (KS _ _ (Just values)) _ _) = atomically $ do
    vals <- readTVar values
    writeTVar values $ M.delete key vals

-- | Lookup a value in the store
lookupValue :: (Ord i) => i -> KademliaInstance i a -> IO (Maybe a)
lookupValue _   (KI _ (KS _ _ Nothing) _ _) = pure Nothing
lookupValue key (KI _ (KS _ _ (Just values)) _ _) = atomically $ do
    vals <- readTVar values
    return . M.lookup key $ vals

-- | Check whether node is banned
isNodeBanned :: Ord i => KademliaInstance i a -> i -> IO Bool
isNodeBanned (KI _ (KS _ banned _) _ _) nid = do
    banSet <- atomically $ readTVar banned
    case M.lookup nid banSet of
        Nothing -> return False
        Just bannedIO -> do
            ban <- bannedIO
            unless ban $ atomically . modifyTVar banned $ M.delete nid
            return ban

-- | Set a way to evaluate, whether given node is banned.
-- Once being evaluated to `False`, given value should neven evaluate to `True` anymore.
--
-- To temporaly ban node `Utils.mkTimer` would be handy.
banNode :: (Serialize i, Ord i) => KademliaInstance i a -> i -> IO Bool -> IO ()
banNode (KI _ (KS sTree banned _) _ _) nid evalBan = atomically $ do
    modifyTVar banned $ M.insert nid evalBan
    modifyTVar sTree $ \t -> T.delete t nid

-- | Start the background process for a KademliaInstance
start :: (Show i, Serialize i, Ord i, Serialize a, Eq a) =>
         KademliaInstance i a -> ReplyQueue i a -> IO ()
start inst rq = do
        startRecvProcess . handle $ inst
        let rChan = timeoutChan rq
            dChan = defaultChan rq
        receivingId <- forkIO . receivingProcess inst rq rChan $ dChan
        pingId <- forkIO . pingProcess inst $ dChan
        spreadId <- forkIO . spreadValueProcess $ inst
        void . forkIO $ backgroundProcess inst dChan [pingId, spreadId, receivingId]

-- | The central process all Replys go trough
receivingProcess :: (Show i, Serialize i, Ord i) =>
       KademliaInstance i a -> ReplyQueue i a -> Chan (Reply i a)
    -> Chan (Reply i a)-> IO ()
receivingProcess inst@(KI h _ _ _) rq replyChan registerChan = forever . (`catch` logError' h) $ do
    reply <- readChan replyChan

    logInfo h $ "Received reply: " ++ show reply

    case reply of
        -- Handle a timed out node
        Timeout registration -> do
            let origin = replyOrigin registration
            let newRegistration = registration { replyTypes = [R_PONG] }

            -- If peer is banned, ignore
            unlessM (isNodeBanned inst origin) $ do
                -- Mark the node as timed out
                pingAgain <- timeoutNode inst origin
                -- If the node should be repinged
                when pingAgain $ do
                    result <- lookupNode inst origin
                    case result of
                        Nothing -> return ()
                        Just node -> do
                            -- Ping the node
                            send h (peer node) PING
                            expect h newRegistration registerChan

        -- Store values in newly encountered nodes that you are the closest to
        Answer (Signal node cmd) -> do
            let originId = nodeId node

            -- If peer is banned, ignore
            unlessM (isNodeBanned inst originId) $ do
                tree <- retrieve sTree

                -- This node is not yet known
                when (not . isJust . T.lookup tree $ originId) $ do
                    let closestKnown = T.findClosest tree originId 1
                    let ownId        = T.extractId tree
                    let self         = node { nodeId = ownId }
                    let bucket       = self:closestKnown
                    -- Find out closest known node
                    let closestId    = nodeId . head . sortByDistanceTo bucket $ originId

                    -- This node can be assumed to be closest to the new node
                    when (ownId == closestId) $ do
                        storedValues <- toList <$> retrieveMaybe values
                        let p = peer node
                        -- Store all stored values in the new node
                        forM_ storedValues (send h p . uncurry STORE)

                case cmd of
                    -- Ping unknown Nodes that were returned by RETURN_NODES.
                    -- Pinging them first is neccessary to prevent disconnected
                    -- nodes from spreading through the networks NodeTrees.
                    (RETURN_NODES _ nodes) -> forM_ nodes $ \retNode -> do
                        result <- lookupNode inst . nodeId $ retNode
                        case result of
                            Nothing -> send (handle inst) (peer retNode) PING
                            _       -> return ()
                    _ -> return ()

        _ -> return ()

    dispatch reply rq
    where
        retrieve f = atomically . readTVar . f . state $ inst
        retrieveMaybe f = maybe (pure mempty) (atomically . readTVar) . f . state $ inst


-- | The actual process running in the background
backgroundProcess :: (Show i, Serialize i, Ord i, Serialize a, Eq a) =>
    KademliaInstance i a -> Chan (Reply i a) -> [ThreadId] -> IO ()
backgroundProcess inst@(KI h _ _ _) chan threadIds = do
    reply <- liftIO . readChan $ chan

    logInfo h $ "Register chan: reply " ++ show reply

    case reply of
        Answer sig -> do
            handleAnswer sig `catch` logError' h
            repeatBP

        -- Kill all other processes and stop on Closed
        Closed -> do
            mapM_ killThread threadIds

            eThreads <- atomically . readTVar . expirationThreads $ inst
            mapM_ killThread $ map snd (M.toList eThreads)

        _ -> logInfo h "-- unknown reply" >> repeatBP
  where
    repeatBP = backgroundProcess inst chan threadIds
    handleAnswer sig = do
        let node = source sig
        -- Handle the signal
        handleCommand (command sig) (peer node) inst
        -- Insert the node into the tree, if it's allready known, it will
        -- be refreshed
        insertNode inst node

-- | Ping all known nodes every five minutes to make sure they are still present
pingProcess :: KademliaInstance i a
            -> Chan (Reply i a)
            -> IO ()
pingProcess (KI h (KS sTree _ _) _ cfg) chan = forever . (`catch` logError' h) $ do
    threadDelay (pingTime cfg)

    tree <- atomically . readTVar $ sTree
    forM_ (T.toList tree) $ \node -> do
        -- Send PING and expect a PONG
        send h (peer node) PING
        expect h (RR [R_PONG] (nodeId node)) $ chan

-- | Store all values stored in the node in the 7 closest known nodes every hour
spreadValueProcess :: (Serialize i)
                   => KademliaInstance i a
                   -> IO ()
spreadValueProcess (KI h (KS sTree _ sValues) _ cfg) = forever . (`catch` logError' h) . void $ do
    threadDelay (storeValueTime cfg)

    case sValues of
        Nothing        -> return ()
        Just valueVars -> do
            values <- atomically . readTVar $ valueVars
            tree <- atomically . readTVar $ sTree

            () <$ mapMWithKey (sendRequests tree) values

    where
          sendRequests tree key val = do
            let closest = T.findClosest tree key 7
            forM_ closest $ \node -> send h (peer node) (STORE key val)

          mapMWithKey :: (k -> v -> IO a) -> Map k v -> IO [a]
          mapMWithKey f m = sequence . map snd . M.toList . M.mapWithKey f $ m

-- | Delete a value after a certain amount of time has passed
expirationProcess :: (Ord i) => KademliaInstance i a -> i -> IO ()
expirationProcess inst@(KI _ _ valueTs cfg) key = do
    -- Map own ThreadId to the key
    myTId <- myThreadId
    oldTId <- atomically $ do
        threadIds <- readTVar valueTs
        writeTVar valueTs $ M.insert key myTId threadIds
        return . M.lookup key $ threadIds

    -- Kill the old timeout thread, if it exists
    when (isJust oldTId) (killThread . fromJust $ oldTId)

    threadDelay (expirationTime cfg)
    deleteValue key inst

-- | Handles the differendt Kademlia Commands appropriately
handleCommand :: (Serialize i, Ord i) =>
    Command i a -> Peer -> KademliaInstance i a -> IO ()
-- Simply answer a PING with a PONG
handleCommand PING peer inst = send (handle inst) peer PONG
-- Return a KBucket with the closest Nodes
handleCommand (FIND_NODE nid) peer inst = returnNodes peer nid inst
-- Insert the value into the values store and start the expiration process
handleCommand (STORE key value) _ inst = do
    insertValue key value inst
    void . forkIO . expirationProcess inst $ key
-- Return the value, if known, or the closest other known Nodes
handleCommand (FIND_VALUE key) peer inst = do
    result <- lookupValue key inst
    case result of
        Just value -> liftIO $ send (handle inst) peer $ RETURN_VALUE key value
        Nothing    -> returnNodes peer key inst
handleCommand _ _ _ = return ()

-- | Return a KBucket with the closest Nodes to a supplied Id
returnNodes :: (Serialize i, Ord i) =>
    Peer -> i -> KademliaInstance i a -> IO ()
returnNodes peer nid (KI h (KS sTree _ _) _ _) = do
    tree           <- atomically . readTVar $ sTree
    rndGen         <- newStdGen
    let closest     = T.findClosest tree nid 7
    let randomNodes = T.pickupNotClosest tree nid 7 (Just closest) rndGen
    let nodes       = closest ++ randomNodes
    liftIO $ send h peer (RETURN_NODES nid nodes)
