{-|
Module      : Network.Kademlia.Instance
Description : Implementation of the KademliaInstance type

"Network.Kademlia.Instance" implements the KademliaInstance type, as well
as all the things that need to happen in the background to get a working
Kademlia instance.
-}

{-# LANGUAGE ViewPatterns #-}

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

import           Control.Arrow               (second)
import           Control.Concurrent          (ThreadId, forkIO, killThread, myThreadId)
import           Control.Concurrent.Chan     (Chan, readChan)
import           Control.Concurrent.STM      (TVar, atomically, modifyTVar, newTVar,
                                              readTVar, readTVarIO, writeTVar)
import           Control.Exception           (catch)
import           Control.Monad               (forM_, forever, forever, unless, void, when)
import           Control.Monad.Extra         (unlessM, whenM)
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
import           Network.Kademlia.Config     (KademliaConfig (..), defaultConfig, k,
                                              usingConfig)
import           Network.Kademlia.Networking (KademliaHandle (..), expect, logError',
                                              send, startRecvProcess)
import           Network.Kademlia.ReplyQueue (Reply (..), ReplyQueue (timeoutChan),
                                              ReplyRegistration (..), ReplyType (..),
                                              defaultChan, dispatch, expectedReply)
import qualified Network.Kademlia.Tree       as T
import           Network.Kademlia.Types      (Command (..), Node (..), Peer (..),
                                              Serialize (..), Signal (..), Timestamp,
                                              sortByDistanceTo)
import           Network.Kademlia.Utils      (threadDelay)
import           Network.Socket              (SockAddr (..), getSocketName, inet_ntoa)
import           System.Random               (newStdGen)

-- | The handle of a running Kademlia Node
data KademliaInstance i a = KI {
      node              :: Node i
    , handle            :: KademliaHandle i a
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
    tree <- atomically $ newTVar (T.create nid `usingConfig` cfg)
    banned <- atomically . newTVar $ M.empty
    values <- if storeValues cfg then Just <$> (atomically . newTVar $ M.empty) else pure Nothing
    threads <- atomically . newTVar $ M.empty
    SockAddrInet portnum hostaddr <- getSocketName (kSock handle)
    host <- inet_ntoa hostaddr
    let ownNode = Node (Peer host portnum) nid
    return $ KI ownNode handle (KS tree banned values) threads cfg

-- | Insert a Node into the NodeTree
insertNode :: (Serialize i, Ord i) => KademliaInstance i a -> Node i -> IO ()
insertNode inst@(KI _ _ (KS sTree _ _) _ cfg) node = do
    currentTime <- floor <$> getPOSIXTime
    unlessM (isNodeBanned inst $ nodeId node) $ atomically $ do
        tree <- readTVar sTree
        writeTVar sTree $ T.insert tree node currentTime `usingConfig` cfg

-- | Signal a Node's timeout and retur wether it should be repinged
timeoutNode :: (Serialize i, Ord i) => KademliaInstance i a -> i -> IO Bool
timeoutNode (KI _ _ (KS sTree _ _) _ cfg) nid = do
    currentTime <- floor <$> getPOSIXTime
    atomically $ do
        tree <- readTVar sTree
        let (newTree, pingAgain) = T.handleTimeout currentTime tree nid `usingConfig` cfg
        writeTVar sTree newTree
        return pingAgain

-- | Lookup a Node in the NodeTree
lookupNode :: (Serialize i, Ord i) => KademliaInstance i a -> i -> IO (Maybe (Node i))
lookupNode (KI _ _ (KS sTree _ _) _ cfg) nid = atomically $ do
    tree <- readTVar sTree
    return $ T.lookup tree nid `usingConfig` cfg

-- | Return all the Nodes an Instance has encountered so far
dumpPeers :: KademliaInstance i a -> IO [(Node i, Timestamp)]
dumpPeers (KI _ _ (KS sTree _ _) _ _) = do
    currentTime <- floor <$> getPOSIXTime
    atomically $ do
        tree <- readTVar sTree
        return . map (second (currentTime -)) . T.toList $ tree

-- | Insert a value into the store
insertValue :: (Ord i) => i -> a -> KademliaInstance i a -> IO ()
insertValue _ _ (KI _ _ (KS _ _ Nothing) _ _)             = return ()
insertValue key value (KI _ _ (KS _ _ (Just values)) _ _) = atomically $ do
    vals <- readTVar values
    writeTVar values $ M.insert key value vals

-- | Delete a value from the store
deleteValue :: (Ord i) => i -> KademliaInstance i a -> IO ()
deleteValue _ (KI _ _ (KS _ _ Nothing) _ _)         = return ()
deleteValue key (KI _ _ (KS _ _ (Just values)) _ _) = atomically $ do
    vals <- readTVar values
    writeTVar values $ M.delete key vals

-- | Lookup a value in the store
lookupValue :: (Ord i) => i -> KademliaInstance i a -> IO (Maybe a)
lookupValue _   (KI _ _ (KS _ _ Nothing) _ _) = pure Nothing
lookupValue key (KI _ _ (KS _ _ (Just values)) _ _) = atomically $ do
    vals <- readTVar values
    return . M.lookup key $ vals

-- | Check whether node is banned
isNodeBanned :: Ord i => KademliaInstance i a -> i -> IO Bool
isNodeBanned (KI _ _ (KS _ banned _) _ _) nid = do
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
banNode (KI _ _ (KS sTree banned _) _ cfg) nid ban = atomically $ do
    modifyTVar banned $ M.insert nid ban
    modifyTVar sTree $ \t -> T.delete t nid `usingConfig` cfg

-- | Shows stored buckets, ordered by distance to this node
viewBuckets :: KademliaInstance i a -> IO [[(Node i, Timestamp)]]
viewBuckets (KI _ _ (KS sTree _ _) _ _) = do
    currentTime <- floor <$> getPOSIXTime
    map (map $ second (currentTime -)) <$> T.toView <$> readTVarIO sTree

-- | Start the background process for a KademliaInstance
start :: (Show i, Serialize i, Ord i, Serialize a, Eq a) =>
         KademliaInstance i a -> IO ()
start inst = do
    let rq = replyQueue $ handle inst
    startRecvProcess . handle $ inst
    receivingId <- forkIO $ receivingProcess inst
    pingId <- forkIO $ pingProcess inst $ defaultChan rq
    spreadId <- forkIO $ spreadValueProcess inst
    void . forkIO $ backgroundProcess inst (defaultChan rq) [pingId, spreadId, receivingId]

-- | The central process all Replys go trough
receivingProcess
    :: (Show i, Serialize i, Ord i)
    => KademliaInstance i a
    -> IO ()
receivingProcess inst@(KI _ h _ _ _) = forever . (`catch` logError' h) $ do
    let rq = replyQueue h
    reply <- readChan $ timeoutChan $ replyQueue h
    let notResponse = not $ isResponse reply
    whenM ((notResponse ||) <$> expectedReply reply rq) $
        receivingProcessDo inst reply rq
  where
    isResponse :: Reply i a -> Bool
    isResponse (Answer (Signal _ PONG))                 = True
    isResponse (Answer (Signal _ (RETURN_VALUE _ _)))   = True
    isResponse (Answer (Signal _ (RETURN_NODES _ _ _))) = True
    isResponse _                                        = False


receivingProcessDo
    :: (Show i, Serialize i, Ord i)
    => KademliaInstance i a
    -> Reply i a
    -> ReplyQueue i a
    -> IO ()
receivingProcessDo inst@(KI _ h _ _ cfg) reply rq = do
    logInfo h $ "Received reply: " ++ show reply

    case reply of
        -- Handle a timed out node
        Timeout registration -> do
            let origin = replyOrigin registration

            -- If peer is banned, ignore
            unlessM (isNodeBanned inst origin) $ do
                -- Mark the node as timed out
                pingAgain <- timeoutNode inst origin
                -- If the node should be repinged
                when pingAgain $ do
                    result <- lookupNode inst origin
                    case result of
                        Nothing   -> return ()
                        Just node -> sendPing h node (defaultChan rq)
            dispatch reply rq -- remove node from ReplyQueue in the last time

        -- Store values in newly encountered nodes that you are the closest to
        Answer (Signal node _) -> do
            let originId = nodeId node

            -- If peer is banned, ignore
            unlessM (isNodeBanned inst originId) $ do
                tree <- retrieve sTree

                -- This node is not yet known
                when (not . isJust $ T.lookup tree originId `usingConfig` cfg) $ do
                    let closestKnown = T.findClosest tree originId 1 `usingConfig` cfg
                    let ownId        = T.extractId tree `usingConfig` cfg
                    let self         = node { nodeId = ownId }
                    let bucket       = self:closestKnown
                    -- Find out closest known node
                    let closestId    = nodeId . head $ sortByDistanceTo bucket originId `usingConfig` cfg

                    -- This node can be assumed to be closest to the new node
                    when (ownId == closestId) $ do
                        storedValues <- toList <$> retrieveMaybe values
                        let p = peer node
                        -- Store all stored values in the new node
                        forM_ storedValues (send h p . uncurry STORE)
                dispatch reply rq
        Closed -> dispatch reply rq -- if Closed message

    where
        retrieve f = atomically . readTVar . f . state $ inst
        retrieveMaybe f = maybe (pure mempty) (atomically . readTVar) . f . state $ inst

-- | The actual process running in the background
backgroundProcess :: (Show i, Serialize i, Ord i, Serialize a, Eq a) =>
    KademliaInstance i a -> Chan (Reply i a) -> [ThreadId] -> IO ()
backgroundProcess inst@(KI _ h _ _ _) chan threadIds = do
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
            insertNode inst node

-- | Ping all known nodes every five minutes to make sure they are still present
pingProcess :: KademliaInstance i a
            -> Chan (Reply i a)
            -> IO ()
pingProcess (KI _ h (KS sTree _ _) _ cfg) chan = forever . (`catch` logError' h) $ do
    threadDelay (pingTime cfg)

    tree <- atomically . readTVar $ sTree
    forM_ (T.toList tree) $ \(fst -> node) -> sendPing h node chan

-- Send PING and expect a PONG
sendPing :: KademliaHandle i a -> Node i -> Chan (Reply i a) -> IO ()
sendPing h node chan = do
    send h (peer node) PING
    expect h (RR [R_PONG] (nodeId node)) $ chan

-- | Store all values stored in the node in the 'k' closest known nodes every hour
spreadValueProcess :: (Serialize i)
                   => KademliaInstance i a
                   -> IO ()
spreadValueProcess (KI _ h (KS sTree _ sValues) _ cfg) = forever . (`catch` logError' h) . void $ do
    threadDelay (storeValueTime cfg)

    case sValues of
        Nothing        -> return ()
        Just valueVars -> do
            values <- atomically . readTVar $ valueVars
            tree <- atomically . readTVar $ sTree

            () <$ mapMWithKey (sendRequests tree) values

    where
          sendRequests tree key val = do
            let closest = T.findClosest tree key (k cfg) `usingConfig` cfg
            forM_ closest $ \node -> send h (peer node) (STORE key val)

          mapMWithKey :: (k -> v -> IO a) -> Map k v -> IO [a]
          mapMWithKey f m = sequence . map snd . M.toList . M.mapWithKey f $ m

-- | Delete a value after a certain amount of time has passed
expirationProcess :: (Ord i) => KademliaInstance i a -> i -> IO ()
expirationProcess inst@(KI _ _ _ valueTs cfg) key = do
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
-- Ping unknown Nodes that were returned by RETURN_NODES.
-- Pinging them first is neccessary to prevent disconnected
-- nodes from spreading through the networks NodeTrees.
handleCommand (RETURN_NODES _ _ nodes) _ inst@KI{..} = forM_ nodes $ \retNode -> do
    result <- lookupNode inst . nodeId $ retNode
    case result of
        Nothing -> sendPing handle retNode (defaultChan $ replyQueue $ handle)
        _       -> return ()
handleCommand _ _ _ = return ()

-- | Return a KBucket with the closest Nodes to a supplied Id
returnNodes :: (Serialize i, Ord i) =>
    Peer -> i -> KademliaInstance i a -> IO ()
returnNodes peer nid (KI ourNode h (KS sTree _ _) _ cfg@KademliaConfig {..}) = do
    tree           <- atomically . readTVar $ sTree
    rndGen         <- newStdGen
    let closest     = T.findClosest tree nid k `usingConfig` cfg
    let randomNodes = T.pickupRandom tree routingSharingN closest rndGen
    -- Must never give an empty list. The networking part assumes that there
    -- will always be at least one node. If there is nothing, then it's not
    -- clear what to send to the peer, and so nothing is sent, and the peer
    -- times out. This causes joinNetwork to time out for the first node to
    -- join (the existing node doesn't know any peers).
    let nodes       = case closest ++ randomNodes of
                          [] -> [ourNode]
                          xs -> xs
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
    nid           = T.extractId (spTree snapshot) `usingConfig` cfg
