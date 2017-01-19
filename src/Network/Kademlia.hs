{-|
Module      : Network.Kademlia
Description : Implementation of the Kademlia DHT
License:      BSD3
Maintainer:   fro_ozen@gmx.de
Stability:    experimental
Portability:  GHC

A haskell implementation of the Kademlia distributed hashtable, an efficient
way to store and lookup values distributed over a P2P network.

The implementation is based on the paper by Petar Maymounkov and David Mazières:
/Kademlia: A Peer-to-peer Information System Based on the XOR Metric/:
<http://pdos.csail.mit.edu/~petar/papers/maymounkov-kademlia-lncs.pdf>

This library aims to be very simple and pleasant to use, with the downside of
deciding some of the implementation details, like timeout intervals and
k-bucket size, for the user.

= How to use it

To get started with this library, first import it. The import has to be
qualified, as the module uses the same function names as some other modules.

> import qualified Network.Kademlia as K

Next, you need to decide on the types you want to use as the values to be stored
in the DHT and the keys to acces them by. As soon as you've decided on them, you
have to make them instances of the "Serialize" typeclass, so they can be sent over
the network.

> import qualified Data.ByteString as B
> import qualified Data.ByteString.Char8 as C
> import Control.Arrow (first)
>
> -- The type this example will use as value
> type Person = data {
>                 age :: Int
>               , name :: String
>               }
>               deriving (Show)
>
> instance K.Serialize Person where
>    toBS = C.pack . show
>    fromBS bs =
>        case (reads :: ReadS Person) . C.unpack $ bs of
>            [] -> Left "Failed to parse Person."
>            (result, rest):_ -> Right (result, C.pack rest)
>
> -- The type this example will use as key for the lookups
> newtype KademliaID = KademliaID B.ByteString
>
> instance K.Serialize KademliaID where
>    toBS (KademliaID bs)
>        | B.length bs >= 5 = B.take 5 bs
>        | otherwise        = error "KademliaID to short!"
>
>    fromBS bs
>        | B.length bs >= 5 = Right . first KademliaID . B.splitAt 5 $ bs
>        | otherwise        = Left "ByteString too short!"
>

As you could see in the example above, for the algorithm to work, you have to make
sure the serialized keys are of a fixed length. There is no such constraint for
the values.

Now you're ready to dive in and use the DHT:

> main = do
>    -- Create the first instance, which will serve as the first node of the
>    -- network
>    firstInstance <- K.create 12345 . KademliaID . C.pack $ "hello"
>
>    -- Create a Node representing the first instance
>    let firstNode = Node (Peer "localhost" 12345) . KademliaID . C.pack $ "hello"
>
>    -- Create the second instance and make it join the network
>    secondInstance <- K.create 12346 . KademliaID . C.pack $ "uAleu"
>    joinResult <- K.joinNetwork secondInstance firstNode
>
>    -- Make sure the joining was successful
>    case joinResult of
>         JoinSuccess -> do
>             -- Store an example value in the network
>             let exampleValue = Person 25 "Alan Turing"
>             K.store secondInstance (KademliaID . C.pack $ "raxqT") exampleValue
>
>             -- Look up the value and it's source
>             (value, source) <- K.lookup firstInstance . KademliaID . C.pack $ "raxqT"
>             print value
>
>         _ -> return ()
>
>    -- Close the instances
>    K.close firstInstance
>    K.close secondInstance

As promised, the usage of the actual DHT is rather easy. There are a few things
to note, though:

    * To join an existing network, you need to know the hostname, listening port
      and id of a node that is already part of that network
    * When you don't need access to the DHT anymore, make sure to close the instances.
      This closes opened sockets and kills the threads running in the background

Another thing to note is, that you are responsible for assigning ids to nodes
and keys to values, as well as making sure these are unique. The Kademlia paper
doesn't propose any measures for this and, as this library is just a
implementation of the system proposed in it, this library doesn't implement
anything to handle this.

-}

module Network.Kademlia
       ( KademliaInstance
       , KademliaConfig(..)
       , KademliaSnapshot
       , create
       , createL
       , createLFromSnapshot
       , defaultConfig
       , close
       , I.lookup
       , I.store
       , I.lookupNode
       , I.joinNetwork
       , viewBuckets
       , dumpPeers
       , banNode
       , isNodeBanned
       , takeSnapshot
       , restoreInstance
       , distance
       , sortByDistanceTo
       , usingKademliaInstance
       , JoinResult(..)
       , Serialize(..)
       , Node(..)
       , Peer(..)
       ) where

import           Network.Kademlia.Config
import           Network.Kademlia.Implementation as I
import           Network.Kademlia.Instance
import           Network.Kademlia.Networking
import           Network.Kademlia.ReplyQueue
import           Network.Kademlia.Types
import qualified Network.Kademlia.Tree as T
import           Prelude                         hiding (lookup)

-- | Create a new KademliaInstance corresponding to a given Id on a given port
create
    :: (Show i, Serialize i, Ord i, Serialize a, Eq a)
    => Int
    -> i
    -> IO (KademliaInstance i a)
create port id' =
    createL port id' defaultConfig (const $ pure ()) (const $ pure ())

-- | Same as create, but with logging
createL
    :: (Show i, Serialize i, Ord i, Serialize a, Eq a)
    => Int
    -> i
    -> KademliaConfig
    -> (String -> IO ())
    -> (String -> IO ())
    -> IO (KademliaInstance i a)
createL port id' cfg logInfo logError = do
    rq <- emptyReplyQueueL logInfo logError
    let lim = msgSizeLimit cfg
    h <- openOnL (show port) id' lim rq logInfo logError
    inst <- newInstance id' cfg h
    start inst rq
    return inst

-- | Create instance from snapshot with logging
createLFromSnapshot
    :: (Show i, Serialize i, Ord i, Serialize a, Eq a)
    => Int
    -> KademliaConfig
    -> KademliaSnapshot i
    -> (String -> IO ())
    -> (String -> IO ())
    -> IO (KademliaInstance i a)
createLFromSnapshot port cfg snapshot logInfo logError = do
    rq <- emptyReplyQueueL logInfo logError
    let lim = msgSizeLimit cfg
    let id' = T.extractId (spTree snapshot) `usingConfig` cfg
    h <- openOnL (show port) id' lim rq logInfo logError
    inst <- restoreInstance cfg h snapshot
    start inst rq
    return inst

-- | Stop a KademliaInstance by closing it
close :: KademliaInstance i a -> IO ()
close = closeK . handle

-- | Run WithConfig action using given kademlia instance
usingKademliaInstance :: WithConfig a -> KademliaInstance i v -> a
usingKademliaInstance f = usingConfig f . config
