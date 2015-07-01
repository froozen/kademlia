{-|
Module      : Protocol
Description : Test for Network.Kademlia.Protocol

Tests specific to Network.Kademlia.Protocol.
-}

module Protocol
    ( parseCheck
    , lengthCheck
    , IdType(..)
    ) where

import Test.QuickCheck

import qualified Data.ByteString as B
import Network.Kademlia.Types
import Network.Kademlia.Protocol
import TestTypes

-- | A signal is the same as its serialized form parsed
parseCheck :: Signal IdType String -> Property
parseCheck s = test . parse (peer . source $ s) . serialize id . command $ s
    where id = nodeId . source $ s
          test (Left err) = counterexample "Parsing failed" False
          test (Right s') = counterexample
            ("Signals differ:\nIn:  " ++ show s ++ "\nOut: "
                 ++ show s' ++ "\n") $ s === s'

-- | A serialized signal's length is no longer than the max. UDP packet size
--   (or at least what I believe to be the max UDP packet size)
lengthCheck :: Signal IdType String -> Property
lengthCheck s = counterexample err $ length <= 1500
    where length = B.length . serialize (nodeId . source $ s) . command $ s
          err = "Serialized signal is too long: " ++ show length ++ " bytes"
