{-|
Module      : Implementation
Description : Tests for Network.Kademlia.Implementation

Tests specific to Network.Kademlia.Implementation.
-}

module Implementation where

import           Test.HUnit                hiding (assert)
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           TestTypes

import qualified Network.Kademlia          as K
import           Network.Kademlia.Instance
import qualified Network.Kademlia.Tree     as T
import           Network.Kademlia.Types

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Monad

import qualified Data.ByteString.Char8     as C
import           Data.Default              (def)
import           Data.Maybe                (fromJust, isJust)

constructNetwork :: IdBunch IdType -> PropertyM IO [KademliaInstance IdType String]
constructNetwork idBunch = run $ do
    let entryNode = Node (Peer "127.0.0.1" 3123) (head . getIds $ idBunch)
    instances <- zipWithM (\p i -> K.create p i def) [3123..] (getIds idBunch)
                        :: IO [KademliaInstance IdType String]

    forM_ (tail instances) (`K.joinNetwork` entryNode)
    return instances

joinCheck :: IdBunch IdType -> Property
joinCheck idBunch = monadicIO $ do
    instances <- constructNetwork idBunch
    present <- run $ do
        mapM_ K.close instances
        mapM filled instances
    assert . and $ present

    where filled inst = do
            tree <- atomically . readTVar . sTree . state $ inst
            return $ (length . T.toList $ tree) >= 7

-- | Make sure ID clashes are detected properly
idClashCheck :: IdType -> IdType -> Property
idClashCheck idA idB = monadicIO $ do
    let peers = map (Peer "127.0.0.1") [1123..]
        ids = [idA, idB, idA]
        entryNode = Node (Peer "127.0.0.1" 1124) idB

    joinResult <- run $ do
        insts@[kiA, _, kiB] <- zipWithM (\p i -> K.create p i def) [1123..] ids
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
    inst <- K.create 1123 idA def :: IO (KademliaInstance IdType String)
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
                _ -> return False

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
          check id result = isJust result && id == (nodeId . fromJust $ result)
