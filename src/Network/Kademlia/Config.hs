module Network.Kademlia.Config
    ( KademliaConfig(..)
    , defaultConfig
    ) where

import           Network.Kademlia.Utils

data KademliaConfig = KademliaConfig {
      expirationTime :: Int -- in seconds
    , storeValueTime :: Int -- in seconds
    , pingTime       :: Int -- in seconds
    , nbLookupNodes  :: Int -- number of nodes to look in parallel during a lookup
                            -- also known as α in kademlia paper
    , msgSizeLimit   :: Int -- upper bound on size of message transfered through
                            -- network; exceeding messages would be splitted
    }

defaultConfig = KademliaConfig
    { expirationTime = hour 1
    , storeValueTime = hour 1
    , pingTime       = minute 5
    , nbLookupNodes  = 3
    , msgSizeLimit   = 1200
    }
