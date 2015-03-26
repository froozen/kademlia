{-|
Module      : Protocol
Description : Test for Network.Kademlia.Protocol

Tests specific to Network.Kademlia.Protocol.
-}

module Protocol
    ( parseCheck
    , IdType(..)
    ) where

import Test.QuickCheck

import Network.Kademlia.Types
import Network.Kademlia.Protocol
import Types

-- | A signal is the same as its serialized form parsed
parseCheck :: Signal IdType String -> Bool
parseCheck s = test . parse (peer s) . serialize (peerId s) . command $ s
    where test (Left _) = False
          test (Right s') = s == s'

