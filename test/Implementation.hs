{-|
Module      : Implementation
Description : Tests for Network.Kademlia.Implementation

Tests specific to Network.Kademlia.Implementation.
-}

module Implementation where

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Monad             (forM, forM_, mapM, zipWithM)
import           Control.Monad.IO.Class    (liftIO)

import qualified Data.ByteString.Char8     as C
import           Data.Maybe                (fromJust, isJust)

import           Test.HUnit                hiding (assert)
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

import qualified Network.Kademlia          as K
import           Network.Kademlia.Instance
import qualified Network.Kademlia.Tree     as T
import           Network.Kademlia.Types

import           TestTypes

constructNetwork :: IdBunch IdType -> PropertyM IO [KademliaInstance IdType String]
constructNetwork idBunch = run $ do
    let entryNode = Node (Peer "127.0.0.1" 3123) (head . getIds $ idBunch)
    instances <- zipWithM K.create [3123..] (getIds idBunch)
                        :: IO [KademliaInstance IdType String]

    forM_ (tail instances) (`K.joinNetwork` entryNode)
    return instances

joinNetworkVerifier :: Int -> IdBunch IdType -> Property
joinNetworkVerifier bucketThreshold idBunch = monadicIO $ do
    instances <- constructNetwork idBunch
    present   <- run $ do
        mapM_ K.close instances
        mapM isBucketFilled instances
    assert $ and present
  where
    isBucketFilled inst = do
        tree <- atomically . readTVar . sTree . state $ inst
        let treeLen = length $ T.toList tree
        return $ treeLen >= bucketThreshold

-- | Checks that nodes contain at least @k@ neighbours in their buckets
joinCheck :: IdBunch IdType -> Property
joinCheck = joinNetworkVerifier 7

-- | Checks that nodes from RETURN_NODES request were added to bucket: [CSL-258][CSL-260]
-- Thus node should contain at least @k + k/2@ nodes.
joinFullCheck :: IdBunch IdType -> Property
joinFullCheck = joinNetworkVerifier 11

-- | Make sure ID clashes are detected properly
idClashCheck :: IdType -> IdType -> Property
idClashCheck idA idB = monadicIO $ do
    let peers = map (Peer "127.0.0.1") [1123..]
        ids = [idA, idB, idA]
        entryNode = Node (Peer "127.0.0.1" 1124) idB

    joinResult <- run $ do
        insts@[kiA, _, kiB] <- zipWithM K.create [1123..] ids
                            :: IO [KademliaInstance IdType String]

        K.joinNetwork kiA $ entryNode
        joinResult <- K.joinNetwork kiB $ entryNode

        mapM_ K.close insts

        return joinResult

    assert $ joinResult == K.IDClash


-- | Make sure an offline peer is detected
nodeDownCheck :: Assertion
nodeDownCheck = do
    let entryNode = Node (Peer "127.0.0.1" 1124) idB
    inst <- K.create 1123 idA :: IO (KademliaInstance IdType String)
    joinResult <- K.joinNetwork inst entryNode
    K.close inst

    assertEqual "" joinResult K.NodeDown

    where idA = IT . C.pack $ "hello"
          idB = IT . C.pack $ "herro"

storeAndLookupCheck :: IdBunch IdType -> IdBunch IdType -> Property
storeAndLookupCheck ids keys = monadicIO $ do
    let keyVal = zip (getIds keys) vals
    instances <- constructNetwork ids

    success <- run $ do
        mapM_ (doStore instances) keyVal

        success <- forM instances $ \inst ->
            and <$> mapM (tryLookup inst) keyVal

        mapM_ K.close instances

        return success

    assert . and $ success

    where vals = take 20 . map (replicate 5) $ ['a'..]
          doStore instances (key, val) = K.store (head instances) key val
          tryLookup inst (key, val) = do
            result <- K.lookup inst key
            case result of
                Just (v, _) -> return $ v == val
                _           -> return False

lookupNodesCheck :: IdBunch IdType -> Property
lookupNodesCheck ids = monadicIO $ do
    instances <- constructNetwork ids

    success <- run $ do
        success <- forM instances $ \inst ->
            and <$> (mapM (tryLookup inst) . getIds $ ids)

        mapM_ K.close instances

        return success

    assert . and $ success

    where tryLookup inst id = check id <$> K.lookupNode inst id
          check id = maybe False ((== id) . nodeId)
