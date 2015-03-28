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
import qualified Test.QuickCheck.Property as P

import Network.Kademlia.Types
import Network.Kademlia.Protocol
import Types

-- | A signal is the same as its serialized form parsed
parseCheck :: Signal IdType String -> P.Result
parseCheck s = test . parse (peer s) . serialize (peerId s) . command $ s
    where test (Left err) = P.failed { P.reason = "Parsing failed: " ++ err }
          test (Right s') = P.result {
                  P.ok = Just (s == s')
                , P.reason = "Signals differ:\nIn:  " ++ show s ++ "\nOut: "
                             ++ show s' ++ "\n"
            }

