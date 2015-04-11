{-|
Module      : Network.Kademlia.Instance
Description : Implementation of the KademliaInstance type

"Network.Kademlia.Instance" implements the KademliaInstance type, as well
as all the things that need to happen in the background to get a working
Kademlia instance.
-}

module Network.Kademlia.Instance
    ( KademliaInstance(..)
    , start
    ) where

import Control.Concurrent
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
    , tree   :: T.NodeTree i
    }

-- | Representation of the data the KademliaProcess carries
data KademliaState i a = KS {
      sTree   :: T.NodeTree i
    , values :: M.Map i a
    }

-- | MonadTransformer context of backgroundProcess
type KademliaProcess i a = ReaderT (KademliaHandle i a) (StateT (KademliaState i a) IO)

-- | Run a KademliaProcess from a KademliaInstance
runKademliaProcess :: KademliaState i a -> KademliaHandle i a
                   -> KademliaProcess i a b -> IO (b, KademliaState i a)
runKademliaProcess state handle process =
    runStateT (runReaderT process handle) state

-- | Start the background process for a KademliaInstance
start :: (Serialize i, Ord i, Serialize a, Eq i, Eq a) =>
    KademliaInstance i a -> IO ()
start inst = do
        chan <- newTChanIO
        startRecvProcess (handle inst) chan
        let state = KS (tree inst) M.empty
        void . forkIO $ backgroundProcess state (handle inst) chan

-- | The actual process running in the background
backgroundProcess :: (Serialize i, Ord i, Serialize a, Eq i, Eq a) =>
    KademliaState i a -> KademliaHandle i a -> TChan (Reply i a) -> IO ()
backgroundProcess state handle chan = do
    (continue, nextState) <- runKademliaProcess state handle $ do
        -- sig <- liftIO . recv $ h
        reply <- liftIO . atomically . readTChan $ chan

        if reply /= Closed
            then do
                let (Answer sig) = reply
                h <- ask
                -- Insert the node into the tree, if it's allready known, it will be
                -- refreshed
                let node = source sig
                lift $ modify $ \s -> s { sTree = T.insert (sTree s) node }

                -- Handle the signal
                handleCommand (command sig) (peer . source $ sig)

                return True
            -- Stop on Closed
            else return False

    when continue $ backgroundProcess nextState handle chan

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
    lift $ modify $ \s -> s { values = M.insert key value (values s) }
-- Return the value, if known, or the closest other known Nodes
handleCommand (FIND_VALUE key) peer = do
    values <- lift $ gets values
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
    tree <- lift $ gets sTree
    let nodes = T.findClosest tree id 7
    liftIO $ send h peer (RETURN_NODES id nodes)
