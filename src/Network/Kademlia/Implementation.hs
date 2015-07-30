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

import Network.Kademlia.Networking
import Network.Kademlia.Instance
import qualified Network.Kademlia.Tree as T
import Network.Kademlia.Types
import Network.Kademlia.ReplyQueue
import Prelude hiding (lookup)
import Control.Monad (forM_, unless, when)
import Control.Monad.Trans.State hiding (state)
import Control.Concurrent.Chan
import Control.Concurrent.STM
import Control.Monad.IO.Class (liftIO)
import Data.List (delete, find, (\\))
import Data.Maybe (isJust, fromJust)


-- | Lookup the value corresponding to a key in the DHT and return it, together
--   with the Node that was the first to answer the lookup
lookup :: (Serialize i, Serialize a, Eq i, Ord i) => KademliaInstance i a -> i
       -> IO (Maybe (a, Node i))
lookup inst id = runLookup go inst id
    where go = startLookup sendS cancel checkSignal

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
                    let cachePeer = peer . head . sortByDistanceTo rest $ id
                    liftIO . send (handle inst) cachePeer . STORE id $ value

                -- Return the value
                return . Just $ (value, origin)

          -- When receiving a RETURN_NODES command, throw the nodes into the
          -- lookup loop and continue the lookup
          checkSignal (Signal _ (RETURN_NODES _ nodes)) =
                continueLookup nodes sendS continue cancel

          -- Continuing always means waiting for the next signal
          continue = waitForReply cancel checkSignal

          -- Send a FIND_VALUE command, looking for the supplied id
          sendS = sendSignal (FIND_VALUE id)

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
store :: (Serialize i, Serialize a, Eq i, Ord i) =>
         KademliaInstance i a -> i -> a -> IO ()
store inst key val = runLookup go inst key
    where go = startLookup sendS end checkSignal

          -- Always add the nodes into the loop and continue the lookup
          checkSignal (Signal _ (RETURN_NODES _ nodes)) =
                continueLookup nodes sendS continue end

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
data JoinResult = JoinSucces | NodeDown | IDClash deriving (Eq, Ord, Show)

-- | Make a KademliaInstance join the network a supplied Node is in
joinNetwork :: (Serialize i, Serialize a, Eq i, Ord i) => KademliaInstance i a
            -> Node i -> IO JoinResult
joinNetwork inst node = ownId >>= runLookup go inst
    where go = do
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
          checkSignal (Signal _ (RETURN_NODES _ nodes)) = do
                tId <- gets targetId
                case find (\node -> nodeId node == tId) nodes of
                    Just _ -> return IDClash
                    _ -> continueLookup nodes sendS continue finish

          -- Continuing always means waiting for the next signal
          continue = waitForReply finish checkSignal

          -- Send a FIND_NODE command, looking up your own id
          sendS node = liftIO ownId >>= flip sendSignal node . FIND_NODE

          -- Return a success, when the operation finished cleanly
          finish = return JoinSucces

-- | Lookup the Node corresponding to the supplied ID
lookupNode :: (Serialize i, Serialize a, Eq i, Ord i) => KademliaInstance i a -> i
           -> IO (Maybe (Node i))
lookupNode inst id = runLookup go inst id
    where go = startLookup sendS end checkSignal

          -- Return Nothing on lookup failure
          end = return Nothing

          -- Check wether the Node we are looking for was found. If so, return
          -- it, otherwise continue the lookup.
          checkSignal (Signal _ (RETURN_NODES _ nodes)) =
                case find (\(Node _ nId) -> nId == id) nodes of
                    Just node -> return . Just $ node
                    _ -> continueLookup nodes sendS continue end

          -- Continuing always means waiting for the next signal
          continue = waitForReply end checkSignal

          -- Send a FIND_NODE command looking for the Node corresponding to the
          -- id
          sendS = sendSignal (FIND_NODE id)

-- | The state of a lookup
data LookupState i a = LookupState {
      inst :: KademliaInstance i a
    , targetId :: i
    , replyChan :: Chan (Reply i a)
    , known :: [Node i]
    , pending :: [Node i]
    , polled :: [Node i]
    }

-- | MonadTransformer context of a lookup
type LookupM i a = StateT (LookupState i a) IO

-- Run a LookupM, returning its result
runLookup :: LookupM i a b -> KademliaInstance i a -> i ->IO b
runLookup lookup inst id = do
    chan <- newChan
    let state = LookupState inst id chan [] [] []

    evalStateT lookup state

-- The initial phase of the normal kademlia lookup operation
startLookup :: (Serialize i, Serialize a, Eq i, Ord i) => (Node i -> LookupM i a ())
            -> LookupM i a b -> (Signal i a -> LookupM i a b) -> LookupM i a b
startLookup sendSignal cancel onSignal = do
    inst <- gets inst
    tree <- liftIO . atomically . readTVar . sTree . state $ inst
    chan <- gets replyChan
    id <- gets targetId

    -- Find the three nodes closest to the supplied id
    case T.findClosest tree id 3 of
            [] -> cancel
            closest -> do
                -- Send a signal to each of the Nodes
                forM_ closest sendSignal

                -- Add them to the list of known nodes. At this point, it will
                -- be empty, therfore just overwrite it.
                modify $ \s -> s { known = closest }

                -- Start the recursive lookup
                waitForReply cancel onSignal

-- Wait for the next reply and handle it appropriately
waitForReply :: (Serialize i, Serialize a, Ord i) => LookupM i a b
             -> (Signal i a -> LookupM i a b) -> LookupM i a b
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
            -- Insert the node into the tree, as it might be a new one or it
            -- would have to be refreshed
            liftIO . insertNode inst $ node

            -- Remove the node from the list of nodes with pending replies
            modify $ \s -> s { pending = delete node sPending }

            -- Call the signal handler
            onSignal sig

        -- On timeout
        Timeout registration -> do
            let id = replyOrigin registration

            -- Find the node corresponding to the id
            --
            -- ReplyQueue guarantees us, that it will be in polled, therefore
            -- we can use fromJust
            let node = fromJust . find (\n -> nodeId n == id) $ polled

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
continueLookup :: (Serialize i, Serialize a, Eq i) => [Node i]
               -> (Node i -> LookupM i a ()) -> LookupM i a b -> LookupM i a b
               -> LookupM i a b
continueLookup nodes sendSignal continue end = do
    known <- gets known
    id <- gets targetId
    pending <- gets pending
    polled <- gets polled

    -- Pick the k closest known nodes, that haven't been polled yet
    let newKnown = take 7 . filter (`notElem` polled) $ nodes ++ known

    -- If there the k closest nodes haven't been polled yet
    closestPolled <- closestPolled newKnown
    if (not . null $ newKnown) && not closestPolled
        then do
            -- Send signal to the closest node, that hasn't
            -- been polled yet
            let next = head . sortByDistanceTo newKnown $ id
            sendSignal next

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
            polled <- gets polled
            closest <- closest known

            return . all (`elem` polled) $ closest

          closest known = do
            id <- gets targetId
            polled <- gets polled

            -- Return the 7 closest nodes, the lookup had contact with
            return . take 7 . sortByDistanceTo (known ++ polled) $ id

-- Send a signal to a node
sendSignal :: (Serialize i, Serialize a, Eq i) => Command i a
          -> Node i -> LookupM i a ()
sendSignal cmd node = do
    h <- fmap handle . gets $ inst
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
                    (FIND_NODE id)  -> RR [R_RETURN_NODES id] (nodeId node)
                    (FIND_VALUE id) ->
                        RR [R_RETURN_NODES id, R_RETURN_VALUE id] (nodeId node)
