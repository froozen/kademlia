module Network.Kademlia.Config
       ( KademliaConfig(..)
       , WithConfigT (..)
       , WithConfig
       , getConfig
       , usingConfigT
       , usingConfig
       , defaultConfig
       , defaultRoutingSharingN
       , defaultK
       ) where

import           Control.Monad.Identity (Identity (runIdentity))
import           Control.Monad.Reader (ReaderT (..), ask)
import           Control.Monad.Trans (MonadTrans)
import           Network.Kademlia.Utils (hour, minute)

data KademliaConfig = KademliaConfig {
      k               :: !Int  -- ^ @k@ nearest heighbours for query. Constant from paper.
    , expirationTime  :: !Int  -- ^ we delete a value after @expirationTime@ seconds has passed
    , storeValueTime  :: !Int  -- ^ we store all values stored in the node in the 'k' closest known nodes every @storeValueTime@ seconds
    , pingTime        :: !Int  -- ^ we ping all known nodes every @pingTime@ seconds to make sure they are still present
    , nbLookupNodes   :: !Int  -- ^ number of nodes to look in parallel during a lookup
                               --   also known as α in kademlia paper
    , msgSizeLimit    :: !Int  -- ^ upper bound on size of message transfered through
                               --   network; exceeding messages would be splitted
    , storeValues     :: !Bool -- ^ when this is False, we don't store anything in this node
    , routingSharingN :: !Int  -- ^ number of nodes from not closest to include int `returnNodes` responses (see [CSL-260])
    , bucketSize      :: !Int  -- ^ bucket size used by Node Storage Tree
    }

newtype WithConfigT m a = WithConfigT
     { getWithConfigT :: ReaderT KademliaConfig m a
     } deriving (Functor, Applicative, Monad, MonadTrans)

type WithConfig a = WithConfigT Identity a

getConfig :: Monad m => WithConfigT m KademliaConfig
getConfig = WithConfigT ask

usingConfigT :: WithConfigT m a -> KademliaConfig -> m a
usingConfigT f cfg = flip runReaderT cfg $ getWithConfigT f

usingConfig :: WithConfig a -> KademliaConfig -> a
usingConfig f cfg = runIdentity $ usingConfigT f cfg

defaultK :: Int
defaultK = 7

defaultRoutingSharingN :: Int
defaultRoutingSharingN = uncurry (+) $ defaultK `divMod` 2

defaultConfig :: KademliaConfig
defaultConfig = KademliaConfig
    { k               = defaultK
    , expirationTime  = hour 1
    , storeValueTime  = hour 1
    , pingTime        = minute 5
    , nbLookupNodes   = 3
    , msgSizeLimit    = 1200
    , storeValues     = True
    , routingSharingN = defaultRoutingSharingN
    , bucketSize      = 4
    }
