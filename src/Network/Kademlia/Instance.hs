{-|
Module      : Network.Kademlia.Instance
Description : Implementation of the KademliaInstance type

"Network.Kademlia.Instance" implements the KademliaInstance type, as well
as all the things that need to happen in the background to get a working
Kademlia instance.
-}

module Network.Kademlia.Instance
    ( KademliaInstance(..)
    ) where

import Network.Kademlia.Networking
import qualified Network.Kademlia.Tree as T
import Network.Kademlia.Types

-- | The handle of a running Kademlia Node
data KademliaInstance i a = KI {
      handle :: KademliaHandle i
    , tree   :: T.NodeTree i
    }
