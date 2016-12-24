module Network.Kademlia.Config
       ( KademliaConfig(..)
       , defaultConfig
       , defaultRoutingSharingN
       , k
       ) where

import           Network.Kademlia.Utils (hour, minute)

data KademliaConfig = KademliaConfig {
      expirationTime  :: !Int  -- ^ in seconds
    , storeValueTime  :: !Int  -- ^ in seconds
    , pingTime        :: !Int  -- ^ in seconds
    , nbLookupNodes   :: !Int  -- ^ number of nodes to look in parallel during a lookup
                              --   also known as α in kademlia paper
    , msgSizeLimit    :: !Int  -- ^ upper bound on size of message transfered through
                              --   network; exceeding messages would be splitted
    , storeValues     :: !Bool -- ^ when this is False, we don't store anything in this node
    , routingSharingN :: !Int -- ^ number of nodes from not closest to include int `returnNodes` responses (see [CSL-260])
    }

defaultRoutingSharingN :: Int
defaultRoutingSharingN = uncurry (+) $ k `divMod` 2

defaultConfig :: KademliaConfig
defaultConfig = KademliaConfig
    { expirationTime = hour 1
    , storeValueTime = hour 1
    , pingTime       = minute 5
    , nbLookupNodes  = 3
    , msgSizeLimit   = 1200
    , storeValues    = True
    , routingSharingN = defaultRoutingSharingN
    }

-- | @k@ nearest heighbours for query. Constant from paper.
--
-- [CSL-310]: This constant is not in 'KademliaConfig' currently because it's usages
-- are not convenient and required in many places. If we want to make it configurable
-- we can implement this as a /compile-time/ constant or put in config in /runtime/.
k :: Int
k = 7
