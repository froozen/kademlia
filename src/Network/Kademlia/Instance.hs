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
    , BanState (..)
    , KademliaSnapshot (..)
    , defaultConfig
    , start
    , newInstance
    , insertNode
    , lookupNode
    , dumpPeers
    , banNode
    , isNodeBanned
    , takeSnapshot
    , takeSnapshot'
    , restoreInstance
    , viewBuckets
    ) where

import           Control.Concurrent          (ThreadId, forkIO, killThread, myThreadId)
import           Control.Concurrent.Chan     (Chan, readChan)
import           Control.Concurrent.STM      (TVar, atomically, modifyTVar, newTVar,
                                              readTVar, readTVarIO, writeTVar)
import           Control.Exception           (catch)
import           Control.Monad               (forM_, forever, forever, unless, void, when)
import           Control.Monad.Extra         (unlessM)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Trans         ()
import           Control.Monad.Trans.Reader  ()
import           Control.Monad.Trans.State   ()
import           Data.Binary                 (Binary)
import           Data.Map                    (Map, toList)
import qualified Data.Map                    as M hiding (Map)
import           Data.Maybe                  (fromJust, isJust)
import           Data.Time.Clock.POSIX       (getPOSIXTime)
import           GHC.Generics                (Generic)
import           System.Random               (newStdGen)

import           Network.Kademlia.Config     (KademliaConfig (..), defaultConfig, k)
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
import System.IO (stderr, hPutStrLn)

-- | The handle of a running Kademlia Node
data KademliaInstance i a = KI {
      handle            :: KademliaHandle i a
    , state             :: KademliaState i a
    , expirationThreads :: TVar (Map i ThreadId)
    , config            :: KademliaConfig
    }

-- | Ban condition for some node
data BanState
    = BanForever
    | BanTill Integer  -- time in microseconds
    | NoBan
    deriving (Eq, Show, Generic)

-- | Representation of the data the KademliaProcess carries
data KademliaState i a = KS {
      sTree  :: TVar (T.NodeTree i)
    , banned :: TVar (Map i BanState)
    , values :: Maybe (TVar (Map i a))
    }

data KademliaSnapshot i = KSP {
      spTree   :: T.NodeTree i
    , spBanned :: Map i BanState
    } deriving (Generic)

instance Binary BanState

instance Binary i => Binary (KademliaSnapshot i)

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
insertNode inst@(KI _ (KS sTree _ _) _ _) node = do
    unlessM (isNodeBanned inst $ nodeId node) $ do
      hPutStrLn stderr $ "Node is not banned" ++ show (peer node)
      atomically $ do
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
        Just b  -> do
            stillBanned <- isBanned b
            unless stillBanned $ atomically . modifyTVar banned $ M.delete nid
            return stillBanned
  where
    isBanned NoBan       = return False
    isBanned BanForever  = return True
    isBanned (BanTill t) = ( < t) . round <$> getPOSIXTime

-- | Mark node as banned
banNode :: (Serialize i, Ord i) => KademliaInstance i a -> i -> BanState -> IO ()
banNode (KI _ (KS sTree banned _) _ _) nid ban = atomically $ do
    modifyTVar banned $ M.insert nid ban
    modifyTVar sTree $ \t -> T.delete t nid

-- | Shows stored buckets, ordered by distance to this node
viewBuckets :: KademliaInstance i a -> IO [[Node i]]
viewBuckets (KI _ (KS sTree _ _) _ _) = T.toView <$> readTVarIO sTree

-- | Start the background process for a KademliaInstance
start :: (Show i, Serialize i, Ord i, Serialize a, Eq a) =>
         KademliaInstance i a -> ReplyQueue i a -> IO ()
start inst rq = do
        startRecvProcess . handle $ inst
        receivingId <- forkIO $ receivingProcess inst rq
        pingId <- forkIO $ pingProcess inst $ defaultChan rq
        spreadId <- forkIO $ spreadValueProcess inst
        void . forkIO $ backgroundProcess inst (defaultChan rq) [pingId, spreadId, receivingId]

-- | The central process all Replys go trough
receivingProcess :: (Show i, Serialize i, Ord i) =>
       KademliaInstance i a -> ReplyQueue i a -> IO ()
receivingProcess inst@(KI h _ _ _) rq = forever . (`catch` logError' h) $ do
    reply <- readChan $ timeoutChan rq

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
                            expect h newRegistration $ defaultChan rq

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
                    (RETURN_NODES _ _ nodes) -> forM_ nodes $ \retNode -> do
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
        Answer sig@(Signal (Node _ nid) _) -> do
            unlessM (isNodeBanned inst nid) $ do
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
    handleAnswer sig@(Signal (Node _ nid) _) =
        unlessM (isNodeBanned inst nid) $ do
            let node = source sig
            -- Handle the signal
            handleCommand (command sig) (peer node) inst
            -- Insert the node into the tree, if it's already known, it will
            -- be refreshed
            liftIO . hPutStrLn stderr $ "INSERT: backgroundProcess" ++ show (peer node)
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

-- | Store all values stored in the node in the 'k' closest known nodes every hour
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
            let closest = T.findClosest tree key k
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
returnNodes peer nid (KI h (KS sTree _ _) _ KademliaConfig {..}) = do
    tree           <- atomically . readTVar $ sTree
    rndGen         <- newStdGen
    let closest     = T.findClosest tree nid k
    let randomNodes = T.pickupRandom tree routingSharingN closest rndGen
    let nodes       = closest ++ randomNodes
    liftIO $ send h peer (RETURN_NODES 1 nid nodes)

-- | Take a current view of `KademliaState`.
takeSnapshot' :: KademliaState i a -> IO (KademliaSnapshot i)
takeSnapshot' (KS tree banned _) = atomically $ do
    spTree   <- readTVar tree
    spBanned <- readTVar banned
    return KSP{..}

-- | Take a current view of `KademliaState`.
takeSnapshot :: KademliaInstance i a -> IO (KademliaSnapshot i)
takeSnapshot = takeSnapshot' . state

-- | Restores instance from snapshot.
restoreInstance :: Serialize i => KademliaConfig -> KademliaHandle i a
                -> KademliaSnapshot i -> IO (KademliaInstance i a)
restoreInstance cfg handle snapshot = do
    inst <- emptyInstance
    let st = state inst
    atomically . writeTVar (sTree  st) $ spTree snapshot
    atomically . writeTVar (banned st) $ spBanned snapshot
    return inst
  where
    emptyInstance = newInstance nid cfg handle
    nid           = T.extractId $ spTree snapshot
