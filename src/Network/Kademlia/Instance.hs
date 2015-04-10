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
import Control.Monad (void, forever)
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class (liftIO)
import System.IO.Error (catchIOError)
import qualified Data.Map as M

import Network.Kademlia.Networking
import qualified Network.Kademlia.Tree as T
import Network.Kademlia.Types

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
runKademliaProcess :: KademliaInstance i a -> KademliaProcess i a b -> IO b
runKademliaProcess (KI handle tree) process =
    evalStateT (runReaderT process handle) (KS tree M.empty)

-- | Start the background process for a KademliaInstance
start :: (Serialize i, Ord i, Serialize a) =>
    KademliaInstance i a -> IO ()
start inst = void $ forkIO $
        -- There is a very high chance that there will arise an Exception,
        -- caused by closing the KademliaHandle in another thread.
        -- This acts as the stopping signal for the background process.
        backgroundProcess inst `catchIOError` \e -> return ()


-- | The actual process running in the background
backgroundProcess :: (Serialize i, Ord i, Serialize a, Eq i) =>
    KademliaInstance i a -> IO ()
backgroundProcess inst = runKademliaProcess inst . forever  $ do
    -- Receive the next signal
    h <- ask
    sig <- liftIO . recv $ h

    -- Insert the node into the tree, if it's allready known, it will be
    -- refreshed
    let node = source sig
    lift $ modify $ \s -> s { sTree = T.insert (sTree s) node }

    -- Handle the signal
    handleCommand (command sig) (peer . source $ sig)

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
