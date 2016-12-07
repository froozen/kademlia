module Network.Kademlia.Config
    ( KademliaConfig(..)
    , defaultConfig
    ) where

import Network.Kademlia.Utils

data KademliaConfig = KademliaConfig {
      expirationTime :: Int -- in seconds
    , storeValueTime :: Int -- in seconds
    , pingTime       :: Int -- in seconds
    }

defaultConfig = KademliaConfig
    { expirationTime = hour 1
    , storeValueTime = hour 1
    , pingTime       = minute 5
    }
