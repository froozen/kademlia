{-|
Module      : Network.Kademlia.Tree
Description : Implementation of the Node Storage Tree

Network.Kademlia.Tree implements the Node Storage Tree used to store
and look up the known nodes.

This module is designed to be used as a qualified import.
-}

{-# LANGUAGE TupleSections #-}

module Network.Kademlia.Tree
       ( NodeTree (..)
       , create
       , insert
       , lookup
       , delete
       , handleTimeout
       , pickupRandom
       , findClosest
       , extractId
       , toView
       , toList
       , fold
       ) where

import           Prelude                 hiding (lookup)

import           Control.Arrow           (second)
import           Control.Monad.Random    (evalRand)
import           Data.Binary             (Binary)
import qualified Data.List               as L (delete, find, genericTake)
import qualified Data.Map                as M
import           GHC.Generics            (Generic)
import           System.Random           (StdGen)
import           System.Random.Shuffle   (shuffleM)

import           Network.Kademlia.Config (KademliaConfig (..), WithConfig, cacheSize,
                                          getConfig, k)
import           Network.Kademlia.Types  (ByteStruct, Node (..), Peer, Serialize (..),
                                          Timestamp, fromByteStruct, sortByDistanceTo,
                                          toByteStruct)

data NodeTree i
    = NodeTree
    { ntOwnId :: ByteStruct
    , ntRoot  :: NodeTreeElem i
    , ntPeers :: M.Map Peer i
    } deriving (Generic)

data PingInfo = PingInfo
    { lastSeenTimestamp :: Timestamp
    } deriving (Generic, Eq)

data NodeTreeElem i
    = Split (NodeTreeElem i) (NodeTreeElem i)
    | Bucket ([(Node i, PingInfo)], [Node i])
    deriving (Generic)

type NodeTreeFunction i a
    = Int
    -> Bool
    -> M.Map Peer i
    -> ([(Node i, PingInfo)], [Node i])
    -> WithConfig a

instance Binary PingInfo

instance Binary i => Binary (NodeTree i)

instance Binary i => Binary (NodeTreeElem i)

-- | Create a NodeTree corresponding to the id
create :: (Serialize i) => i -> WithConfig (NodeTree i)
create nid = NodeTree <$>
    toByteStruct nid <*>
    pure (Bucket ([], [])) <*>
    pure mempty

-- | Lookup a node within a NodeTree
lookup :: (Serialize i, Eq i) => NodeTree i -> i -> WithConfig (Maybe (Node i))
lookup tree nid = applyAt tree nid f
  where
    f _ _ _ = return . L.find (idMatches nid) . map fst . fst

-- | Delete a Node corresponding to a supplied Id from a NodeTree
delete :: (Serialize i, Eq i) => NodeTree i -> Peer -> WithConfig (NodeTree i)
delete tree peer = flip (maybe $ pure tree) (M.lookup peer (ntPeers tree)) $ \nid ->
    modifyAt tree nid (f nid)
  where
    f nid _ _ peers (nodes, cache) =
        let filtered = filter (not . idMatches nid . fst) $ nodes
        in pure (Bucket (filtered, cache), M.delete peer peers)

-- | Handle a timed out node by incrementing its timeoutCount and deleting it
--  if the count exceeds the limit. Also, return wether it's reasonable to ping
--  the node again.
handleTimeout :: (Serialize i, Eq i) => Timestamp -> NodeTree i -> Peer -> WithConfig (NodeTree i, Bool)
handleTimeout currentTime tree pr = flip (maybe $ pure (tree, False)) (M.lookup pr (ntPeers tree)) $ \nid -> do
    KademliaConfig{..} <- getConfig
    let acceptDiff = (fromIntegral pingLimit) * (fromIntegral pingTime)
    let f _ _ peers (nodes, cache) = return $ case L.find (idMatches nid . fst) nodes of
            -- Delete a node that exceeded the limit. Don't contact it again
            --   as it is now considered dead
            Just x@(_, PingInfo lastSeen)
                | currentTime - lastSeen > acceptDiff ->
                    (Bucket (L.delete x $ nodes, cache), M.delete pr peers, False)
            -- Increment the timeoutCount
            Just x@(n, PingInfo lastSeen) ->
                 (Bucket ((n, PingInfo lastSeen) : L.delete x nodes, cache), peers, True)
            -- Don't contact an unknown node a second time
            Nothing -> (Bucket (nodes, cache), peers, False)
    modifyApplyAt tree nid f

-- | Refresh the node corresponding to a supplied Id by placing it at the first
--   index of it's KBucket and reseting its timeoutCount and timestamp, then return a Bucket
--   NodeTreeElem
refresh :: Eq i => Node i -> Timestamp -> ([(Node i, PingInfo)], [Node i]) -> NodeTreeElem i
refresh node currentTimestamp (nodes, cache) =
         Bucket (case L.find (idMatches (nodeId node) . fst) nodes of
            Just x@(n, _) -> (n, PingInfo currentTimestamp) : L.delete x nodes
            _             -> nodes
            , cache)

-- | Insert a node into a NodeTree
insert :: (Serialize i, Eq i) => NodeTree i -> Node i -> Timestamp -> WithConfig (NodeTree i)
insert tree node@Node{nodeId=nid,..} currentTime = do
    k <- k <$> getConfig
    cacheSize <- cacheSize <$> getConfig
    let needsSplit depth valid _ (nodes, _) = do
          maxDepth <- (subtract 1) . length <$> toByteStruct nid
          return $
            -- A new node will be inserted
            node `notElem` map fst nodes &&
            -- The bucket is full
            length nodes >= k &&
            -- The bucket may be split
            -- @georgeee: I peered at this code for ~30-40 mins.
            --   I clearly don't understand what was the reason to introduce `depth < 5`.
            --   Perhaps some kind of +\- 1, to not care about corner case
           (depth < 5 || valid) && depth <= maxDepth

        doInsert _ _ peers b@(nodes, cache)
          -- Refresh an already existing node
          | node `elem` map fst nodes = pure (refresh node currentTime b, peers)
          -- Simply insert the node, if the bucket isn't full
          | length nodes < k = pure (Bucket ((node, PingInfo currentTime):nodes, cache), M.insert peer nid peers)
          -- Move the node to the first spot, if it's already cached
          | node `elem` cache = pure (Bucket (nodes, node : L.delete node cache), peers)
          -- Cache the node and drop older ones, if necessary
          | otherwise = pure (Bucket (nodes, node : take (cacheSize - 1) cache), peers)
    r <- applyAt tree nid needsSplit
    if r then
        -- Split the tree before inserting, when it makes sense
        let splitTree = split tree . nodeId $ node
        in (\t -> insert t node currentTime) =<< splitTree
    -- Insert the node
    else modifyAt tree nid doInsert

-- | Split the KBucket the specified id would reside in into two and return a
--   Split NodeTreeElem
split :: (Serialize i) => NodeTree i -> i -> WithConfig (NodeTree i)
split tree splitId = modifyAt tree splitId g
    where g depth _ peers (nodes, cache) = do
            (leftNodes, rightNodes) <- splitBucket depth fst nodes
            (leftCache, rightCache) <- splitBucket depth id cache
            pure (
                Split
                    (Bucket (leftNodes, leftCache))
                    (Bucket (rightNodes, rightCache)),
                peers)

          -- Recursivly split the nodes into two buckets
          splitBucket _ _ []     = return ([], [])
          splitBucket i f (n:ns) = do
              bs <- toByteStruct . nodeId . f $ n
              let bit = bs !! i
              (left, right) <- splitBucket i f ns
              return $ if bit
                       then (left, n:right)
                       else (n:left, right)

-- | Returns @n@ random nodes from @all \\ ignoredList@.
pickupRandom
    :: (Eq i)
    => NodeTree i
    -> Int
    -> [Node i]
    -> StdGen
    -> [Node i]
pickupRandom _ 0 _ _ = []
pickupRandom tree n ignoreList randGen =
    let treeList      = toList tree
        notIgnored    = filter (`notElem` ignoreList) $ map fst treeList
        shuffledNodes = evalRand (shuffleM notIgnored) randGen
    in L.genericTake n shuffledNodes

-- | Find the k closest Nodes to a given Id
findClosest
    :: (Serialize i)
    => NodeTree i
    -> i
    -> Int
    -> WithConfig [Node i]
findClosest (NodeTree idStruct treeElem _) nid n = do
    let chooseClosest nodes = take n <$> (sortByDistanceTo nodes $ nid)

        -- This function is partial for the same reason as in modifyAt
        --
        -- Take the n closest nodes
        go _ _ (Bucket (nodes, _))
          | length nodes <= n = return $ map fst nodes
          | otherwise         = chooseClosest $ map fst nodes
        -- Take the closest nodes from the left child first, if those aren't
        -- enough, take the rest from the right
        go (_:is) (False:ts) (Split left right) = do
          result <- go is ts left
          if length result == n
          then return result
          else (result ++) <$> go is ts right
        -- Take the closest nodes from the right child first, if those aren't
        -- enough, take the rest from the left
        go (_:is) (True:ts) (Split left right) = do
          result <- go is ts right
          if length result == n
          then return result
          else (result ++) <$> go is ts left
        go _ _ _ = error "Fundamental error in @go@ function in 'findClosest'"

    targetStruct <- toByteStruct nid
    chooseClosest =<< go idStruct targetStruct treeElem

-- Extract original Id from NodeTree
extractId :: (Serialize i) => NodeTree i -> WithConfig i
extractId (NodeTree nid _ _) = fromByteStruct nid

-- | Helper function used for KBucket manipulation
idMatches :: (Eq i) => i -> Node i -> Bool
idMatches nid node = nid == nodeId node

-- | Turn the NodeTree into a list of buckets, ordered by distance to origin node
toView :: NodeTree i -> [[(Node i, Timestamp)]]
toView (NodeTree bs treeElems _) = go bs treeElems []
  where
    -- If the bit is 0, go left, then right
    go (False:is) (Split left right) = go is left . go is right
    -- Else go right first
    go (True:is)  (Split left right) = go is right . go is left
    go _          (Split _    _    ) = error "toView: unexpected Split"
    go _          (Bucket (b, _))    = (map (second lastSeenTimestamp) b :)

-- | Turn the NodeTree into a list of nodes
toList :: NodeTree i -> [(Node i, Timestamp)]
toList = concat . toView

-- | Fold over the buckets
fold :: ([Node i] -> a -> a) -> a -> NodeTree i -> a
fold f start (NodeTree _ treeElems _) = go start treeElems
    where go a (Split left right) = let a' = go a left in go a' right
          go a (Bucket b)         = f (map fst . fst $ b) a

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

-- There are three similar functions,
-- its go down by tree using passed NodeId.
-- When its reach leaf with bucket its apply, modify or apply and modify stored bucket.

-- | Modify the position in the tree where the supplied id would be
modifyAt
    :: (Serialize i)
    => NodeTree i
    -> i
    -> NodeTreeFunction i (NodeTreeElem i, M.Map Peer i)
    -> WithConfig (NodeTree i)
modifyAt tree nid f =
    fst <$> modifyApplyAt tree nid (\a b c d -> md1 <$> f a b c d)
  where
    md1 (x, y) = (x, y, ())

-- | Apply a function to the bucket the supplied id would be located in
applyAt :: (Serialize i) => NodeTree i -> i -> NodeTreeFunction i a -> WithConfig a
applyAt (NodeTree idStruct treeElem peers) nid f = do
    targetStruct <- toByteStruct nid
    go idStruct targetStruct 0 True treeElem
  where
    -- This function is partial for the same reason as in modifyAt
    --
    -- Apply the function
    go _ _ depth valid (Bucket b) = f depth valid peers b
    -- If the bit is a 0, go left
    go (i:is) (False:ts) depth valid (Split left _) =
        go is ts (depth + 1) (valid && not i) left
    -- Otherwise, continue to the right
    go (i:is) (True:ts) depth valid (Split _ right) =
        go is ts (depth + 1) (valid && i) right
    go _ _ _ _ _ = error "Fundamental error in @go@ function in 'applyAt'"

-- | Modify and apply a function at the position in the tree where the
-- supplied id would be
modifyApplyAt ::
    (Serialize i)
    => NodeTree i
    -> i
    -> NodeTreeFunction i (NodeTreeElem i, M.Map Peer i, a)
    -> WithConfig (NodeTree i, a)
modifyApplyAt (NodeTree idStruct treeElem peers) nid f = do
    targetStruct <- toByteStruct nid
    (newElems, mpeers, val) <- go idStruct targetStruct 0 True treeElem
    pure (NodeTree idStruct newElems mpeers, val)
  where
    -- This function is partial, but we know that there will alwasys be a
    -- bucket at the end. Therefore, we don't have to check for empty
    -- ByteStructs
    --
    -- Apply the function to the position of the bucket
    go _ _ depth valid (Bucket b) = f depth valid peers b
    -- If the bit is a 0, go left
    go (i:is) (False:ts) depth valid (Split left right) = do
        (new, mpeers, val) <- go is ts (depth + 1) (valid && not i) left
        pure (Split new right, mpeers, val)
    -- Otherwise, continue to the right
    go (i:is) (True:ts) depth valid (Split left right) = do
        (new, mpeers, val) <- go is ts (depth + 1) (valid && i) right
        pure (Split left new, mpeers, val)
    go _ _ _ _ _ = error "Fundamental error in @go@ function in 'bothAt'"

