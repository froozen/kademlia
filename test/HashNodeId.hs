{-# LANGUAGE ViewPatterns #-}

{-|
Module      : HashNodeId
Description : Tests for Network.Kademlia.HashNodeId

Tests specific to Network.Kademlia.HashNodeId.
-}

module HashNodeId
       ( fromToBadHashId
       , fromToHashId
       , notVerifyBadHashId
       , toFromBadHashId
       , toFromHashId
       , verifyHashId
       ) where

import           Data.Either                 (isLeft)

import           Network.Kademlia.HashNodeId (HashId (..), verifyAddress)
import           Network.Kademlia.Types      (fromBS, toBS)

import           TestTypes                   (BadHashId (..))

-- | Checks whether toBS converts 'HashNodeId' into 'ByteString' correctly
toFromHashId :: HashId -> Bool
toFromHashId hid = hid == (either err fst . fromBS . toBS $ hid)
  where
    err = error "toFromHashId: conversion failed"

-- | Checks whether fromBS converts 'ByteString' into 'HashNodeId' correctly
fromToHashId :: HashId -> Bool
fromToHashId hid =
    let bhid = toBS hid
        err = error "fromToHashId: conversion failed"
    in bhid == (toBS @HashId . either err fst . fromBS $ bhid)

-- | Checks whether toBS converts 'HashNodeId' into 'ByteString' correctly
toFromBadHashId :: BadHashId -> Bool
toFromBadHashId (getBadId -> hid) =
    isLeft . fromBS @HashId . toBS $ hid

-- | Checks whether fromBS converts 'ByteString' into 'HashNodeId' correctly
fromToBadHashId :: BadHashId -> Bool
fromToBadHashId (getBadId -> hid) =
    let bhid = toBS hid
    in isLeft . fromBS @HashId $ bhid

-- | Checks whether a valid 'HashId' is successfully verified
verifyHashId :: HashId -> Bool
verifyHashId (HashId b) = verifyAddress b

-- | Checks whether an invalid 'HashId' is unsuccessfully verified
notVerifyBadHashId :: BadHashId -> Bool
notVerifyBadHashId (BadHashId (HashId b)) = not . verifyAddress $ b
