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
    ) where

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.STM
import Control.Monad (void, forever, when)
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class (liftIO)
import System.IO.Error (catchIOError)
import qualified Data.Map as M

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

-- | MonadTransformer context of backgroundProcess
type KademliaProcess i a = ReaderT (KademliaHandle i a) (StateT (KademliaState i a) IO)

-- | Run a KademliaProcess from a KademliaInstance
runKademliaProcess :: KademliaInstance i a -> KademliaProcess i a b
                   -> IO (b, KademliaState i a)
runKademliaProcess (KI handle state) process =
    runStateT (runReaderT process handle) state

-- | Modify a part of the KademliaState
modifyP :: (KademliaState i a -> TVar b) -> (b -> b) -> KademliaProcess i a ()
modifyP take f = do
    tVar <- lift . gets $ take
    liftIO . atomically $ do
        x <- readTVar tVar
        writeTVar tVar $ f x

-- | Read the current state of a part of the KademliaState
readP :: (KademliaState i a -> TVar b) -> KademliaProcess i a b
readP take = do
    tVar <- lift . gets $ take
    liftIO . atomically . readTVar $ tVar

-- | Start the background process for a KademliaInstance
start :: (Serialize i, Ord i, Serialize a, Eq i, Eq a) =>
    KademliaInstance i a -> IO ()
start inst = do
        chan <- newChan
        startRecvProcess (handle inst) chan
        void . forkIO $ backgroundProcess inst chan

-- | The actual process running in the background
backgroundProcess :: (Serialize i, Ord i, Serialize a, Eq i, Eq a) =>
    KademliaInstance i a -> Chan (Reply i a) -> IO ()
backgroundProcess inst chan = do
    (continue, nextState) <- runKademliaProcess inst $ do
        reply <- liftIO . readChan $ chan

        if reply /= Closed
            then do
                let (Answer sig) = reply
                let node = source sig

                -- Insert the node into the tree, if it's allready known, it will
                -- be refreshed
                modifyP sTree $ \tree -> T.insert tree node

                -- Handle the signal
                handleCommand (command sig) (peer node)

                return True
            -- Stop on Closed
            else return False

    when continue $ backgroundProcess inst chan

-- | Handles the differendt Kademlia Commands appropriately
handleCommand :: (Serialize i, Eq i, Ord i, Serialize a) =>
    Command i a -> Peer -> KademliaProcess i a ()
-- Simply answer a PING with a PONG
handleCommand PING peer = do
    h <- ask
    liftIO $ send h peer PONG
-- Return a KBucket with the closest Nodes
handleCommand (FIND_NODE id) peer = returnNodes peer id
-- Insert the value into the values Map
handleCommand (STORE key value) _ =
    modifyP values $ \vals -> M.insert key value vals
-- Return the value, if known, or the closest other known Nodes
handleCommand (FIND_VALUE key) peer = do
    values <- readP values
    case M.lookup key values of
        Just value -> do
            h <- ask
            liftIO $ send h peer $ RETURN_VALUE key value
        Nothing    -> returnNodes peer key
handleCommand _ _ = return ()

-- | Return a KBucket with the closest Nodes to a supplied Id
returnNodes :: (Serialize i, Eq i, Ord i, Serialize a) =>
    Peer -> i -> KademliaProcess i a ()
returnNodes peer id = do
    h <- ask
    tree <- readP sTree
    let nodes = T.findClosest tree id 7
    liftIO $ send h peer (RETURN_NODES id nodes)
