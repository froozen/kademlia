{-|
Module      : Network.Kademlia
Description : Implementation of the Kademlia DHT

The "Network.Kademlia" module implements the popular Distributed Hash Table of
the same name.

The original concept was proposed by Petar Maymounkov and David Mazières.
(For the details, consult their paper
/Kademlia: A Peer-to-peer Information System Based on the XOR Metric/:
<http://pdos.csail.mit.edu/~petar/papers/maymounkov-kademlia-lncs.pdf>)
-}

module Network.Kademlia
    ( KademliaInstance
    , create
    , close
    , lookup
    , store
    ) where

import Network.Kademlia.Networking
import Network.Kademlia.Instance
import qualified Network.Kademlia.Tree as T
import Network.Kademlia.Types
import Prelude hiding (lookup)
import Control.Monad (void)
import Data.Binary

-- | Create a new Kademlia Instance corresponding to a given Id on a given port
create :: (Binary i, Ord i, Eq i, Binary a, Eq a) =>
    Int -> i -> IO (KademliaInstance i a)
create port id = do
    h <- openOn (show port) id
    let tree = T.create id
    let inst = KI h tree
    start inst
    return inst

-- | Stop a Kademlia Instance by closing it
close :: KademliaInstance i a -> IO ()
close = closeK . handle

-- | Lookup the value corresponding to an Id in the Kademlia Network, using
--   a running Kademlia Instance
lookup :: (Binary i, Binary a) =>
           KademliaInstance i a -> i -> IO (Maybe a)
lookup = undefined

-- | Store the value corresponding to a Key in the Kademlia Network, using a
--   running Kademlia Instance
store :: (Binary i, Binary a) =>
          KademliaInstance i a -> i -> a -> IO ()
store = undefined
