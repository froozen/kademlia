{-|
Module      : ReplyQueue
Description : Tests for Network.Kademlia.ReplyQueue

Tests specific to Network.Kademlia.ReplyQueue
-}

module ReplyQueue where

import Test.QuickCheck
import Test.QuickCheck.Monadic

import Control.Concurrent.STM

import Network.Kademlia.ReplyQueue
import Network.Kademlia.Types

import TestTypes

-- | Check wether registered reply handlers a used and removed after usage
repliesCheck :: Signal IdType String -> Property
repliesCheck sig = monadicIO $ do
    (sig2, use, removed) <- run . atomically $ do
        rq <- emptyReplyQueue
        chan <- newTChan :: STM (TChan (Signal IdType String))

        let reg = toRegistration sig
        case reg of
            -- Discard the test case
            Nothing -> return (Nothing, False, False)
            Just reg -> do
                register reg rq chan

                dispatch sig rq

                empty <- isEmptyTChan chan
                if empty
                    then return (Nothing, True, False)
                    else do
                        sig2 <- (readTChan chan :: STM (Signal IdType String))
                        removed <- dispatch sig rq
                        return $ (Just sig2, True, removed)

    pre use

    assert $ sig2 /= Nothing

    let (Just unwrapped) = sig2
    assert $ unwrapped == sig

    assert . not $ removed

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
