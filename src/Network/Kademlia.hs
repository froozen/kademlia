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
    , I.lookup
    , I.store
    , I.joinNetwork
    ) where

import Network.Kademlia.Networking
import Network.Kademlia.Instance
import qualified Network.Kademlia.Tree as T
import Network.Kademlia.Types
import Network.Kademlia.ReplyQueue
import Network.Kademlia.Implementation as I
import Prelude hiding (lookup)
import Control.Monad (void, forM_)
import Control.Concurrent.Chan
import Control.Concurrent.STM

-- | Create a new Kademlia Instance corresponding to a given Id on a given port
create :: (Serialize i, Ord i, Serialize a, Eq a, Eq i) =>
    Int -> i -> IO (KademliaInstance i a)
create port id = do
    h <- openOn (show port) id
    inst <- newInstance id h
    start inst
    return inst

-- | Stop a Kademlia Instance by closing it
close :: KademliaInstance i a -> IO ()
close = closeK . handle
