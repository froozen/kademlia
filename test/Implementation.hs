{-|
Module      : Implementation
Description : Tests for Network.Kademlia.Implementation

Tests specific to Network.Kademlia.Implementation.
-}

module Implementation where

import Test.QuickCheck
import Test.QuickCheck.Monadic
import TestTypes

import qualified Network.Kademlia as K
import qualified Network.Kademlia.Tree as T
import Network.Kademlia.Types
import Network.Kademlia.Instance

import Control.Monad
import Control.Applicative
import Control.Concurrent.STM
import Data.Maybe (isJust, fromMaybe)

constructNetwork :: IdBunch IdType -> PropertyM IO [KademliaInstance IdType String]
constructNetwork idBunch = do
    let entryNode = Node (Peer "127.0.0.1" 1123) (head . getIds $ idBunch)
    instances <- run (zipWithM K.create [1123..] (getIds idBunch)
                        :: IO [KademliaInstance IdType String])

    run $ forM_ (tail instances) (`K.joinNetwork` entryNode)
    return instances

joinCheck :: IdBunch IdType -> Property
joinCheck idBunch = monadicIO $ do
    instances <- constructNetwork idBunch
    run $ mapM_ K.close instances

    present <- run $ mapM filled instances
    assert . and $ present

    where filled inst = do
            tree <- atomically . readTVar . sTree . state $ inst
            return $ treeSize tree >= 7
          treeSize = foldr treeCount 0
          treeCount (_, segment) total = total + (length . fromMaybe [] $ segment)

storeAndLookupCheck :: IdBunch IdType -> IdBunch IdType -> Property
storeAndLookupCheck ids keys = monadicIO $ do
    let keyVal = zip (getIds keys) vals
    instances <- constructNetwork ids
    run $ mapM_ (doStore instances) keyVal

    success <- run $ forM instances $ \inst ->
        and <$> mapM (tryLookup inst) keyVal

    run $ mapM_ K.close instances

    assert . and $ success

    where vals = take 20 . map (replicate 5) $ ['a'..]
          doStore instances (key, val) = K.store (head instances) key val
          tryLookup inst (key, val) = do
            result <- K.lookup inst key
            case result of
                Just v -> return $ v == val
                _ -> return False
