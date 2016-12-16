module Network.Kademlia.Utils
       ( threadDelay
       , mkTimer
       , hour
       , minute
       ) where

import qualified Control.Concurrent    (threadDelay)
import           Data.Time.Clock.POSIX (getPOSIXTime)

-- thread delay in seconds
threadDelay :: Int -> IO ()
threadDelay n = Control.Concurrent.threadDelay (n * 1000000)

-- Creates @IO Bool@ value, which reports whether given amount of time has elapsed.
mkTimer :: Int -> IO (IO Bool)
mkTimer timeout = do
    start <- getPOSIXTime
    return $ do
        cur <- getPOSIXTime
        return $ cur < start + fromIntegral (timeout * 1000000)

hour, minute :: Num a => a -> a
hour   n = 3600 * n
minute n = 60   * n
