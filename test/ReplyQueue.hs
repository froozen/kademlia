{-|
Module      : ReplyQueue
Description : Tests for Network.Kademlia.ReplyQueue

Tests specific to Network.Kademlia.ReplyQueue
-}

module ReplyQueue where

import Test.QuickCheck
import Test.QuickCheck.Monadic

import Control.Concurrent.Chan
import Control.Concurrent.STM

import Network.Kademlia.ReplyQueue
import Network.Kademlia.Types

import TestTypes

-- | Check wether registered reply handlers a used
repliesCheck :: Signal IdType String -> Property
repliesCheck sig = monadicIO $ do
    rq <- run $ emptyReplyQueue
    chan <- run $ (newChan :: IO (Chan (Reply IdType String)))

    let reg = toRegistration sig
    case reg of
        -- Discard the test case
        Nothing -> pre False
        Just reg -> do
            run $ register [reg] rq chan

            run $ dispatch sig rq

            contents <- run $ getChanContents chan
            assert . not . null $ contents

            let sig2 = head contents

            assert $ sig2 /= Closed
            let (Answer unwrapped) = sig2
            assert $ unwrapped == sig

-- | Check wether registered reply handlers are removed after usage
removedCheck :: Signal IdType String -> Property
removedCheck sig = monadicIO $ do
    rq <- run $ emptyReplyQueue
    chan <- run $ (newChan :: IO (Chan (Reply IdType String)))

    let reg = toRegistration sig
    case reg of
        -- Discard the test case
        Nothing -> pre False
        Just reg -> do
            run $ register [reg] rq chan

            run $ dispatch sig rq

            removed <- run $ dispatch sig rq
            assert . not $ removed

-- | Check wether flushing works as expected
flushCheck :: Signal IdType String -> Property
flushCheck sig = monadicIO $ do
    rq <- run $ emptyReplyQueue
    chan <- run $ (newChan :: IO (Chan (Reply IdType String)))

    let reg = toRegistration sig
    case reg of
        -- Discard the test case
        Nothing -> pre False
        Just reg -> do
            run $ register [reg] rq chan
            run $ flush rq

            sent <- run $ dispatch sig rq

            assert . not $ sent

-- | Convert a Signal into its ReplyRegistration representation
toRegistration :: Signal i a -> Maybe (ReplyRegistration i)
toRegistration sig = case rType . command $ sig of
                        Nothing -> Nothing
                        Just rt -> Just (ReplyRegistration rt origin)
    where origin = nodeId . source $ sig

          rType :: Command i a -> Maybe (ReplyType i)
          rType  PONG               = Just  R_PONG
          rType (RETURN_VALUE id _) = Just (R_RETURN_VALUE id)
          rType (RETURN_NODES id _) = Just (R_RETURN_NODES id)
          rType _ = Nothing
