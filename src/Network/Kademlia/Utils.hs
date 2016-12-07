module Network.Kademlia.Utils
    ( threadDelay
    , hour
    , minute
    ) where

import qualified Control.Concurrent (threadDelay)

-- thread delay in seconds
threadDelay :: Int -> IO ()
threadDelay n = Control.Concurrent.threadDelay (n * 1000000)

hour n = 3600 * n
minute n = 60 * n

