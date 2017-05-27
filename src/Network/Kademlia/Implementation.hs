{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

{-|
Module      : Network.Kademlia.Implementation
Description : The details of the lookup algorithm

"Network.Kademlia.Implementation" contains the actual implementations of the
different Kademlia Network Algorithms.
-}

module Network.Kademlia.Implementation
    ( lookup
    , store
    , joinNetwork
    , JoinResult(..)
    , Network.Kademlia.Implementation.lookupNode
    ) where

import           Prelude                     hiding (lookup)

import           Control.Concurrent.Chan     (Chan, newChan, readChan)
import           Control.Concurrent.STM      (atomically, readTVar)
import           Control.Monad               (forM_, unless)
import           Control.Monad.Extra         (unlessM)
import           Control.Monad.IO.Class      (MonadIO (..))
import           Control.Monad.Trans.State   (StateT, evalStateT, gets, modify)
import           Data.List                   (delete, find, (\\))
import qualified Data.Map                    as M
import           Data.Maybe                  (catMaybes)
import           Data.Word                   (Word8)

import           Network.Kademlia.Config     (KademliaConfig (..), usingConfig)
import           Network.Kademlia.Instance   (KademliaInstance (..), KademliaState (..),
                                              insertNode, isNodeBanned, peersToNodeIds)
import           Network.Kademlia.Networking (expect, send)
import           Network.Kademlia.ReplyQueue hiding (logError, logInfo)
import qualified Network.Kademlia.Tree       as T
import           Network.Kademlia.Types      (Command (..), Node (..), Peer,
                                              Serialize (..), Signal (..),
                                              sortByDistanceTo)

----------------------------------------------------------------------------
-- Interface
----------------------------------------------------------------------------

-- | Lookup the value corresponding to a key in the DHT and return it, together
--   with the Node that was the first to answer the lookup
lookup
    :: (Serialize i, Serialize a, Ord i)
    => KademliaInstance i a -> i -> IO (Maybe (a, Node i))
lookup inst nid = runLookup go inst nid
  where
    go = startLookup (config inst) sendS cancel checkSignal

    -- Return Nothing on lookup failure
    cancel = pure Nothing

    -- When receiving a RETURN_VALUE command, finish the lookup, then
    -- cache the value in the closest peer that didn't return it and
    -- finally return the value
    checkSignal (Signal origin (RETURN_VALUE _ value)) = do
        -- Abuse the known list for saving the peers that are *known* to
        -- store the value
        modify $ \s -> s { known = [peer origin] }

        -- Finish the lookup, recording which nodes returned the value
        finish

        -- Store the value in the closest peer that didn't return the
        -- value
        known <- gets known
        polled <- gets polled
        rest <- resolveNodeIds inst (polled \\ known)
        unless (null rest) $ do
            let cachePeer = peer . head $ sortByDistanceTo rest nid `usingConfig` config inst
            liftIO . send (handle inst) cachePeer . STORE nid $ value

        -- Return the value
        pure . Just $ (value, origin)

    -- When receiving a RETURN_NODES command, throw the nodes into the
    -- lookup loop and continue the lookup
    checkSignal (Signal _ (RETURN_NODES _ _ nodes)) =
        continueLookup nodes sendS continue cancel
    checkSignal _ = error "Fundamental error in unhandled query @lookup@"

    -- Continuing always means waiting for the next signal
    continue = waitForReply cancel checkSignal

    -- Send a FIND_VALUE command, looking for the supplied id
    sendS = sendSignal (FIND_VALUE nid)

    -- As long as there still are pending requests, wait for the next one
    finish = do
        pending <- gets pending
        unless (null pending) $ waitForReply (return ()) finishCheck

    -- Record the nodes which return the value
    finishCheck (Signal origin (RETURN_VALUE _ _)) = do
        known <- gets known
        modify $ \s -> s { known = peer origin:known }
        finish
    finishCheck _ = finish

-- | Store assign a value to a key and store it in the DHT
store
    :: (Serialize i, Serialize a, Ord i)
    => KademliaInstance i a -> i -> a -> IO ()
store inst key val = runLookup go inst key
  where
    go = startLookup (config inst) sendS end checkSignal

    -- Always add the nodes into the loop and continue the lookup
    checkSignal (Signal _ (RETURN_NODES _ _ nodes)) =
        continueLookup nodes sendS continue end
    checkSignal _ = error "Meet unknown signal in store"

    -- Continuing always means waiting for the next signal
    continue = waitForReply end checkSignal

    -- Send a FIND_NODE command, looking for the node corresponding to the
    -- key
    sendS = sendSignal (FIND_NODE key)

    -- Run the lookup as long as possible, to make sure the nodes closest
    -- to the key were polled.
    end = do
        polled <- gets polled >>= resolveNodeIds inst

        unless (null polled) $ do
            let h = handle inst
                k' = k $ config inst
                -- Don't select more than k peers
                peerNum = if length polled > k' then k' else length polled
                -- Select the peers closest to the key
                storePeers =
                    map peer . take peerNum $ sortByDistanceTo polled key `usingConfig` config inst

            -- Send them a STORE command
            forM_ storePeers $
                \storePeer -> liftIO . send h storePeer . STORE key $ val

-- | The different possibel results of joinNetwork
data JoinResult
    = JoinSuccess
    | NodeDown
    | IDClash
    | NodeBanned
    deriving (Eq, Ord, Show)

-- | Make a KademliaInstance join the network a supplied Node is in
joinNetwork
    :: (Serialize i, Serialize a, Ord i)
    => KademliaInstance i a -> Peer -> IO JoinResult
joinNetwork inst peer = ownId >>= runLookup go inst
  where
    go = do
        -- If node is banned, quit
        banned <- liftIO $ isNodeBanned inst peer
        if banned then pure NodeBanned
        else do
            -- Poll the supplied node
            --liftIO $ putStrLn $ "join: sending to " ++ show (peer node)
            sendS peer
            -- Run a normal lookup from thereon out
            waitForReply nodeDown checkSignal

    -- No answer to the first signal means, that that Node is down
    nodeDown = return NodeDown

    -- Retrieve your own id
    ownId = (`usingConfig` config inst) . T.extractId <$>
            (atomically . readTVar .  sTree . state $ inst)

    -- Also insert all returned nodes to our bucket (see [CSL-258])
    checkSignal (Signal _ (RETURN_NODES _ _ nodes)) = do
        -- Check whether the own id was encountered. If so, return a IDClash
        -- error, otherwise, continue the lookup.
        -- Commented out due to possibility of bug (like when node reconnects)
        -- tId <- gets targetId
        -- case find (\retNode -> nodeId retNode == tId) nodes of
        --     Just _ -> return IDClash
        --     _      -> continueLookup nodes sendS continue finish
        continueLookup nodes sendS continue finish

    checkSignal _ = error "Unknow signal for @joinNetwork@"

    -- Continuing always means waiting for the next signal
    continue = waitForReply finish checkSignal

    -- Send a FIND_NODE command, looking up your own id
    sendS toPeer = liftIO ownId >>= flip sendSignal toPeer . FIND_NODE

    -- Return a success, when the operation finished cleanly
    finish = return JoinSuccess

-- | Lookup the Node corresponding to the supplied ID
lookupNode
    :: forall i a . (Serialize i, Serialize a, Ord i)
    => KademliaInstance i a -> i -> IO (Maybe (Node i))
lookupNode inst nid = runLookup go inst nid
  where
    go :: LookupM i a (Maybe (Node i))
    go = startLookup (config inst) sendS end checkSignal

    -- Return empty list on lookup failure
    end :: LookupM i a (Maybe (Node i))
    end = return Nothing

    -- Check wether the Node we are looking for was found. There are two cases after receiving:
    -- * If we didn't found node then continue lookup
    -- * otherwise: return found node
    -- Also insert all returned nodes to our tree (see [CSL-258])
    checkSignal :: Signal i v -> LookupM i a (Maybe (Node i))
    checkSignal (Signal _ (RETURN_NODES _ _ nodes)) = do
        let targetNode = find ((== nid) . nodeId) nodes
        case targetNode of
            Nothing  -> continueLookup nodes sendS continue end
            justNode -> pure justNode
    checkSignal _ = end  -- maybe it should be `panic` if we get some other return result

    -- Continuing always means waiting for the next signal
    continue :: LookupM i a (Maybe (Node i))
    continue = waitForReply end checkSignal

    -- Send a FIND_NODE command looking for the Node corresponding to the id
    sendS :: Peer -> LookupM i a ()
    sendS = sendSignal (FIND_NODE nid)

----------------------------------------------------------------------------
-- Lookup internal
----------------------------------------------------------------------------

-- | The state of a lookup
data LookupState i a = LookupState
    { inst      :: !(KademliaInstance i a)
    , targetId  :: !i
    , replyChan :: !(Chan (Reply i a))
    , known     :: ![Peer]
    , pending   :: !(M.Map Peer Word8)
    , polled    :: ![Peer]
    }

-- | MonadTransformer context of a lookup
type LookupM i a = StateT (LookupState i a) IO

-- Run a LookupM, returning its result
runLookup :: Ord i => LookupM i a b -> KademliaInstance i a -> i -> IO b
runLookup lookupM inst nid = do
    chan <- newChan
    let state = LookupState inst nid chan mempty mempty mempty

    evalStateT lookupM state

-- The initial phase of the normal kademlia lookup operation
startLookup
    :: (Serialize i, Serialize a, Ord i)
    => KademliaConfig
    -> (Peer -> LookupM i a ())
    -> LookupM i a b
    -> (Signal i a -> LookupM i a b)
    -> LookupM i a b
startLookup cfg signalAction cancel onSignal = do
    inst  <- gets inst
    tree  <- liftIO . atomically . readTVar . sTree . state $ inst
    nid   <- gets targetId

    -- Find the three nodes closest to the supplied id
    case T.findClosest tree nid (nbLookupNodes cfg) `usingConfig` cfg of
            [] -> cancel
            (map peer -> closest) -> do
                -- Add them to the list of known nodes. At this point, it will
                -- be empty, therefore just overwrite it.
                modify $ \s -> s { known = closest }

                -- Send a signal to each of the Nodes
                forM_ closest signalAction

                -- Start the recursive lookup
                waitForReply cancel onSignal

-- Wait for the next reply and handle it appropriately
waitForReply
    :: (Serialize i, Serialize a, Ord i)
    => LookupM i a b
    -> (Signal i a -> LookupM i a b)
    -> LookupM i a b
waitForReply cancel onSignal = do
    chan <- gets replyChan
    inst <- gets inst

    result <- liftIO . readChan $ chan
    case result of
        -- If there was a reply
        Answer sig@(Signal node@Node{..} cmd) -> do
            banned <- liftIO $ isNodeBanned inst peer

            if banned then
                -- Ignore message from banned node, wait for another message
                removeFromEverywhere peer >> continueIfMorePending
            else do
                -- Insert the node into the tree, as it might be a new one or it
                -- would have to be refreshed
                liftIO . insertNode inst $ node

                case cmd of
                    RETURN_NODES n nid _ -> do
                        toRemove <- maybe True ((>= n) . (+1)) <$> gets (M.lookup peer . pending)
                        if toRemove then removeFromPending peer
                        else do
                            modify $ \s -> s { pending = M.adjust (+1) peer $ pending s }
                            let h = handle inst
                                reg = RR [R_RETURN_NODES nid] peer
                            liftIO $ expect h reg chan
                    _ -> removeFromPending peer
                -- Call the signal handler
                onSignal sig

        -- On timeout
        Timeout registration -> do
            let pr = replyOrigin registration

            removeFromEverywhere pr
            continueIfMorePending

        Closed -> cancel
  where
    -- Remove the node from the list of nodes with pending replies
    removeFromPending peer = modify $ \s -> s { pending = M.delete peer $ pending s }
    -- Remove every trace of the node's existance
    removeFromEverywhere peer = modify $ \s -> s
        { pending = M.delete peer $ pending s
        , known = delete peer $ known s
        , polled = delete peer $ polled s
        }
    -- Continue, if there still are pending responses
    continueIfMorePending = do
        updatedPending <- gets pending
        if not . null $ updatedPending
            then waitForReply cancel onSignal
            else cancel

-- Decide wether, and which node to poll and react appropriately.
--
-- This is the meat of kademlia lookups
continueLookup
    :: (Serialize i, Eq i)
    => [Node i]
    -> (Peer -> LookupM i a ())
    -> LookupM i a b
    -> LookupM i a b
    -> LookupM i a b
continueLookup nodes signalAction continue end = do
    inst    <- gets inst
    known   <- gets known >>= resolveNodeIds inst
    nid     <- gets targetId
    pending <- gets pending
    polled  <- gets polled

    -- Pick the k closest known nodes, that haven't been polled yet
    let newKnown = take (k $ config inst) .
                   (`usingConfig` config inst) .
                   (`sortByDistanceTo` nid) .
                   filter ((`notElem`polled) . peer) $
                   nodes ++ known

    -- Check if k closest nodes have been polled already
    polledNeighbours <- allClosestPolled inst newKnown
    if (not . null $ newKnown) && not polledNeighbours then do
        -- Send signal to the closest node, that hasn't
        -- been polled yet
        let next = peer $ head $ sortByDistanceTo newKnown nid `usingConfig` config inst
        signalAction next

        -- Update known
        modify $ \s -> s { known = map peer newKnown }

        -- Continue the lookup
        continue
    -- If there are still pending replies
    else if not . null $ pending then
    -- Wait for the pending replies to finish
        continue
    else
    -- Stop recursive lookup
        end
  where
    allClosestPolled :: Serialize i => KademliaInstance i a -> [Node i] -> LookupM i a Bool
    allClosestPolled inst known = do
        polled       <- gets polled
        closestKnown <- closest inst known
        pure . all ((`elem` polled) . peer) $ closestKnown

    closest :: Serialize i => KademliaInstance i a -> [Node i] -> LookupM i a [Node i]
    closest inst known = do
        cid    <- gets targetId
        polled <- gets polled >>= resolveNodeIds inst

        -- Return the k closest nodes, the lookup had contact with
        pure . take (k $ config inst) $
            sortByDistanceTo (known ++ polled) cid `usingConfig` config inst

-- Send a signal to a node
sendSignal
    :: Ord i
    => Command i a
    -> Peer
    -> LookupM i a ()
sendSignal cmd peer = do
    inst <- gets inst

    -- Not interested in results from banned node
    unlessM (liftIO $ isNodeBanned inst $ peer) $ do
        let h = handle inst
        chan <- gets replyChan
        polled <- gets polled
        pending <- gets pending

        -- Send the signal
        liftIO . send h peer $ cmd

        -- Expect an appropriate reply to the command
        liftIO . expect h regs $ chan

        -- Mark the node as polled and pending
        modify $ \s -> s {
              polled = peer:polled
            , pending = M.insert peer 0 pending
            }

    -- Determine the appropriate ReplyRegistrations to the command
  where
    regs = case cmd of
        FIND_NODE nid  -> RR [R_RETURN_NODES nid] peer
        FIND_VALUE nid ->
            RR [R_RETURN_NODES nid, R_RETURN_VALUE nid] peer
        _               -> error "Unknown command at @sendSignal@"

resolveNodeIds
    :: MonadIO m
    => KademliaInstance i a -> [Peer] -> m [Node i]
resolveNodeIds inst peers = catMaybes <$> liftIO (peersToNodeIds inst peers)
