{-# LANGUAGE ScopedTypeVariables #-}

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
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Trans.State   (StateT, evalStateT, gets, modify)
import           Data.List                   (delete, find, (\\))
import           Data.Maybe                  (fromJust)

import           Network.Kademlia.Config     (KademliaConfig (..))
import           Network.Kademlia.Instance   (KademliaInstance (..), KademliaState (..),
                                              insertNode, isNodeBanned)
import           Network.Kademlia.Networking (expect, send)
import           Network.Kademlia.ReplyQueue hiding (logError, logInfo)
import qualified Network.Kademlia.Tree       as T
import           Network.Kademlia.Types      (Command (..), Node (..), Serialize (..),
                                              Signal (..), sortByDistanceTo)


-- | Lookup the value corresponding to a key in the DHT and return it, together
--   with the Node that was the first to answer the lookup
lookup :: (Serialize i, Serialize a, Ord i) => KademliaInstance i a -> i
       -> IO (Maybe (a, Node i))
lookup inst nid = runLookup go inst nid
    where go = startLookup (config inst) sendS cancel checkSignal

          -- Return Nothing on lookup failure
          cancel = return Nothing

          -- When receiving a RETURN_VALUE command, finish the lookup, then
          -- cache the value in the closest peer that didn't return it and
          -- finally return the value
          checkSignal (Signal origin (RETURN_VALUE _ value)) = do
                -- Abuse the known list for saving the peers that are *known* to
                -- store the value
                modify $ \s -> s { known = [origin] }

                -- Finish the lookup, recording which nodes returned the value
                finish

                -- Store the value in the closest peer that didn't return the
                -- value
                known <- gets known
                polled <- gets polled
                let rest = polled \\ known
                unless (null rest) $ do
                    let cachePeer = peer . head . sortByDistanceTo rest $ nid
                    liftIO . send (handle inst) cachePeer . STORE nid $ value

                -- Return the value
                return . Just $ (value, origin)

          -- When receiving a RETURN_NODES command, throw the nodes into the
          -- lookup loop and continue the lookup
          checkSignal (Signal _ (RETURN_NODES _ nodes)) =
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
                modify $ \s -> s { known = origin:known }
                finish
          finishCheck _ = finish

-- | Store assign a value to a key and store it in the DHT
store :: (Serialize i, Serialize a, Ord i) =>
         KademliaInstance i a -> i -> a -> IO ()
store inst key val = runLookup go inst key
    where go = startLookup (config inst) sendS end checkSignal

          -- Always add the nodes into the loop and continue the lookup
          checkSignal (Signal _ (RETURN_NODES _ nodes)) =
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
            polled <- gets polled

            unless (null polled) $ do
                let h = handle inst
                    -- Don't select more than 7 peers
                    peerNum = if length polled > 7 then 7 else length polled
                    -- Select the peers closest to the key
                    storePeers =
                        map peer . take peerNum . sortByDistanceTo polled $ key

                -- Send them a STORE command
                forM_ storePeers $
                    \storePeer -> liftIO . send h storePeer . STORE key $ val

-- | The different possibel results of joinNetwork
data JoinResult
    = JoinSucces
    | NodeDown
    | IDClash
    | NodeBanned
    deriving (Eq, Ord, Show)

-- | Make a KademliaInstance join the network a supplied Node is in
joinNetwork :: (Serialize i, Serialize a, Ord i) => KademliaInstance i a
            -> Node i -> IO JoinResult
joinNetwork inst node = ownId >>= runLookup go inst
    where go = do
            -- If node is banned, quit
            banned <- liftIO $ isNodeBanned inst $ nodeId node
            if banned
                then return NodeBanned
                else do
                    -- Poll the supplied node
                    sendS node
                    -- Run a normal lookup from thereon out
                    waitForReply nodeDown checkSignal

          -- No answer to the first signal means, that that Node is down
          nodeDown = return NodeDown

          -- Retrieve your own id
          ownId =
            fmap T.extractId . atomically . readTVar .  sTree . state $ inst

          -- Check wether the own id was encountered. If so, return a IDClash
          -- error, otherwise, continue the lookup.
          -- Also insert all returned nodes to our bucket (see [CSL-258])
          checkSignal (Signal _ (RETURN_NODES _ nodes)) = do
                forM_ nodes $ liftIO . insertNode inst
                tId <- gets targetId
                case find (\retNode -> nodeId retNode == tId) nodes of
                    Just _ -> return IDClash
                    _      -> continueLookup nodes sendS continue finish
          checkSignal _ = error "Unknow signal for @joinNetwork@"

          -- Continuing always means waiting for the next signal
          continue = waitForReply finish checkSignal

          -- Send a FIND_NODE command, looking up your own id
          sendS sendNode = liftIO ownId >>= flip sendSignal sendNode . FIND_NODE

          -- Return a success, when the operation finished cleanly
          finish = return JoinSucces

-- | Lookup the Node corresponding to the supplied ID
lookupNode :: forall i a .
              (Serialize i, Serialize a, Ord i)
           => KademliaInstance i a
           -> i
           -> IO (Maybe (Node i))
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
    checkSignal (Signal _ (RETURN_NODES _ nodes)) = do
        forM_ nodes $ liftIO . insertNode inst
        let targetNode = find ((== nid) . nodeId) nodes
        case targetNode of
            Nothing  -> continueLookup nodes sendS continue end
            justNode -> return justNode
    checkSignal _ = end  -- maybe it should be `panic` if we get some other return result

    -- Continuing always means waiting for the next signal
    continue :: LookupM i a (Maybe (Node i))
    continue = waitForReply end checkSignal

    -- Send a FIND_NODE command looking for the Node corresponding to the id
    sendS :: Node i -> LookupM i a ()
    sendS = sendSignal (FIND_NODE nid)

-- | The state of a lookup
data LookupState i a = LookupState
    { inst      :: !(KademliaInstance i a)
    , targetId  :: !i
    , replyChan :: !(Chan (Reply i a))
    , known     :: ![Node i]
    , pending   :: ![Node i]
    , polled    :: ![Node i]
    }

-- | MonadTransformer context of a lookup
type LookupM i a = StateT (LookupState i a) IO

-- Run a LookupM, returning its result
runLookup :: LookupM i a b -> KademliaInstance i a -> i ->IO b
runLookup lookupM inst nid = do
    chan <- newChan
    let state = LookupState inst nid chan [] [] []

    evalStateT lookupM state

-- The initial phase of the normal kademlia lookup operation
startLookup :: (Serialize i, Serialize a, Ord i)
            => KademliaConfig
            -> (Node i -> LookupM i a ())
            -> LookupM i a b -> (Signal i a -> LookupM i a b) -> LookupM i a b
startLookup cfg signalAction cancel onSignal = do
    inst  <- gets inst
    tree  <- liftIO . atomically . readTVar . sTree . state $ inst
    nid   <- gets targetId

    -- Find the three nodes closest to the supplied id
    case T.findClosest tree nid (nbLookupNodes cfg) of
            [] -> cancel
            closest -> do
                -- Send a signal to each of the Nodes
                forM_ closest signalAction

                -- Add them to the list of known nodes. At this point, it will
                -- be empty, therfore just overwrite it.
                modify $ \s -> s { known = closest }

                -- Start the recursive lookup
                waitForReply cancel onSignal

-- Wait for the next reply and handle it appropriately
waitForReply :: (Serialize i, Serialize a, Ord i)
             => LookupM i a b
             -> (Signal i a -> LookupM i a b)
             -> LookupM i a b
waitForReply cancel onSignal = do
    chan <- gets replyChan
    sPending <- gets pending
    known <- gets known
    inst <- gets inst
    polled <- gets polled

    result <- liftIO . readChan $ chan
    case result of
        -- If there was a reply
        Answer sig@(Signal node _) -> do
            banned <- liftIO $ isNodeBanned inst $ nodeId node

            if banned
                -- Ignore message from banned node, wait for another message
                then cancel
                else do
                    -- Insert the node into the tree, as it might be a new one or it
                    -- would have to be refreshed
                    liftIO . insertNode inst $ node

                    -- Remove the node from the list of nodes with pending replies
                    modify $ \s -> s { pending = delete node sPending }

                    -- Call the signal handler
                    onSignal sig

        -- On timeout
        Timeout registration -> do
            let nid = replyOrigin registration

            -- Find the node corresponding to the id
            --
            -- ReplyQueue guarantees us, that it will be in polled, therefore
            -- we can use fromJust
            let node = fromJust . find (\n -> nodeId n == nid) $ polled

            -- Remove every trace of the node's existance
            modify $ \s -> s {
                  pending = delete node sPending
                , known = delete node known
                , polled = delete node polled
                }

            -- Continue, if there still are pending responses
            updatedPending <- gets pending
            if not . null $ updatedPending
                then waitForReply cancel onSignal
                else cancel

        Closed -> cancel

-- Decide wether, and which node to poll and react appropriately.
--
-- This is the meat of kademlia lookups
continueLookup :: (Serialize i, Eq i) => [Node i]
               -> (Node i -> LookupM i a ()) -> LookupM i a b -> LookupM i a b
               -> LookupM i a b
continueLookup nodes signalAction continue end = do
    known   <- gets known
    nid     <- gets targetId
    pending <- gets pending
    polled  <- gets polled

    -- Pick the k closest known nodes, that haven't been polled yet
    let newKnown = take 7 . filter (`notElem` polled) $ nodes ++ known

    -- If there the k closest nodes haven't been polled yet
    polledNeighbours <- closestPolled newKnown
    if (not . null $ newKnown) && not polledNeighbours
        then do
            -- Send signal to the closest node, that hasn't
            -- been polled yet
            let next = head . sortByDistanceTo newKnown $ nid
            signalAction next

            -- Update known
            modify $ \s -> s { known = newKnown }

            -- Continue the lookup
            continue

        -- If there are still pending replies
        else if not . null $ pending
            -- Wait for the pending replies to finish
            then continue
            -- Stop recursive lookup
            else end

    where closestPolled known = do
            polled       <- gets polled
            closestKnown <- closest known

            return . all (`elem` polled) $ closestKnown

          closest known = do
            cid    <- gets targetId
            polled <- gets polled

            -- Return the 7 closest nodes, the lookup had contact with
            return . take 7 . sortByDistanceTo (known ++ polled) $ cid

-- Send a signal to a node
sendSignal :: Ord i
           => Command i a
           -> Node i
           -> LookupM i a ()
sendSignal cmd node = do
    inst <- gets inst

    -- Not interested in results from banned node
    unlessM (liftIO $ isNodeBanned inst $ nodeId node) $ do
        let h = handle inst
        chan <- gets replyChan
        polled <- gets polled
        pending <- gets pending

        -- Send the signal
        liftIO . send h (peer node) $ cmd

        -- Expect an appropriate reply to the command
        liftIO . expect h regs $ chan

        -- Mark the node as polled and pending
        modify $ \s -> s {
              polled = node:polled
            , pending = node:pending
            }

    -- Determine the appropriate ReplyRegistrations to the command
    where regs = case cmd of
                    (FIND_NODE nid)  -> RR [R_RETURN_NODES nid] (nodeId node)
                    (FIND_VALUE nid) ->
                        RR [R_RETURN_NODES nid, R_RETURN_VALUE nid] (nodeId node)
                    _               -> error "Unknown command at @sendSignal@"
