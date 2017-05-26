import           Control.Arrow             (first)
import           Control.Monad             (when)
import           Control.Monad.Random      (Rand, RandomGen, evalRand, evalRandIO,
                                            getRandom)
import           Control.Monad.Trans       (lift)
import qualified Control.Monad.Trans.State as S
import           Data.Binary               (Binary (..), decodeOrFail, encode, getWord8,
                                            putWord8)
import qualified Data.ByteString           as B
import qualified Data.ByteString.Char8     as C
import           Data.ByteString.Lazy      (fromStrict, toStrict)
import           GHC.Conc                  (threadDelay)
import           Network                   (PortNumber)
import qualified Network.Kademlia          as K
import           System.Environment        (getArgs)
import           System.Random             (mkStdGen)
import           System.Random.Shuffle     (shuffleM)

data Pong = Pong
          deriving (Eq, Show)

instance K.Serialize Pong where
    toBS = toBSBinary
    fromBS = fromBSBinary

type KademliaValue = Pong
newtype KademliaID = KademliaID B.ByteString
                   deriving (Show, Eq, Ord)

type KademliaInstance = K.KademliaInstance KademliaID KademliaValue
instance K.Serialize KademliaID where
    toBS (KademliaID bs)
        | B.length bs >= kIdLength = B.take kIdLength bs
        | otherwise                = error $ "KademliaID to short!"

    fromBS bs
        | B.length bs >= kIdLength = Right . first KademliaID . B.splitAt kIdLength $ bs
        | otherwise                = Left "ByteString too short!"

instance Binary Pong where
    put _ = putWord8 1
    get = do
        w <- getWord8
        if w == 1
        then pure Pong
        else fail "no parse pong"

data NodeConfig = NodeConfig
    { ncInstance  :: KademliaInstance
    , ncNodeIndex :: Int
    , ncPort      :: PortNumber
    , ncKey       :: KademliaID
    , ncBctEdges  :: Int
    }

type NodeMode r = S.StateT NodeConfig IO r

data Command = Dump String -- ^ Dump peers list to the file with name log/<name><node id>.log
             | Sleep Int   -- ^ Sleep for a given amount of seconds

kIdLength :: Integral a => a
kIdLength = 20

randomSeed :: Integral a => a
randomSeed = 123

toBSBinary :: Binary b => b -> B.ByteString
toBSBinary = toStrict . encode

fromBSBinary :: Binary b => B.ByteString -> Either String (b, B.ByteString)
fromBSBinary bs =
    case decodeOrFail $ fromStrict bs of
        Left (_, _, errMsg)  -> Left errMsg
        Right (rest, _, res) -> Right (res, toStrict rest)

parseCommand :: String -> Command
parseCommand s = case words s of
    ["sleep", secs] -> Sleep $ read secs
    ["dump", name]  -> Dump name
    _               -> error $ "couldn't parse command " ++ s

generateByteString :: (RandomGen g) => Int -> Rand g B.ByteString
generateByteString len = C.pack <$> sequence (replicate len getRandom)

generateKey :: (RandomGen g) => B.ByteString -> Rand g B.ByteString
generateKey prefix = B.append prefix <$> generateByteString (kIdLength - B.length prefix)

generateKeys :: (RandomGen g) => Int -> [Int] -> Rand g [B.ByteString]
generateKeys prefixLength [n1, n2, n3] = do
    n1keys <- generate n1 (C.pack "")
    prefix <- generateByteString prefixLength
    n2keys <- generate n2 prefix
    n3keys <- generate n3 prefix
    return $ n1keys ++ n2keys ++ n3keys
  where
    generate count prefix = sequence $ replicate count $ generateKey prefix
generateKeys _ _            = error "there should be exactly 3 groups"

generatePorts :: [Int] -> [Int]
generatePorts [n1, n2, n3] = [3000 .. 3000 + n1 + n2 + n3]
generatePorts _            = error "there should be exactly 3 groups"

listToStr :: Show s => [s] -> String
listToStr = unlines . map show

takeRandom :: Int -> [a] -> IO [a]
takeRandom x l | x > length l = takeRandom (length l) l
               | otherwise    = take x <$> evalRandIO (shuffleM l)

executeCommand :: Command -> NodeMode ()
executeCommand (Sleep t) = do
    lift $ putStrLn "Executing sleep command"
    lift $ threadDelay $ t * 1000000
executeCommand (Dump name) = do
    lift $ putStrLn "Executing dump command"
    inct <- ncInstance <$> S.get
    idx <- ncNodeIndex <$> S.get
    bctEdges <- ncBctEdges <$> S.get
    buckets <- lift $ K.viewBuckets inct
    ourNode <- K.Node . K.Peer "127.0.0.1" . ncPort <$> S.get <*> (ncKey <$> S.get)
    edges <- lift . fmap concat
                  . mapM (\l -> takeRandom bctEdges l)
                  . filter (\l -> ourNode `notElem` l && not (null l))
                  $ map (map fst) buckets
    lift $ appendFile ("log/" ++ name ++ show idx ++ ".log") $
        (listToStr $ concat buckets) ++ (unlines $ map (("edge " ++) . show) edges)

connectToPeer :: KademliaInstance -> PortNumber -> B.ByteString -> IO K.JoinResult
connectToPeer inst peerPort _ = K.joinNetwork inst (K.Peer "127.0.0.1" peerPort)

main :: IO ()
main = do
    (k : rSharing : pingTime :
     prefixLen : nodeIndex :
     peerIndex : bctEdges : groups) <- map read <$> getArgs
    let
        ports     = generatePorts groups
        keys      = evalRand (generateKeys prefixLen groups) (mkStdGen randomSeed)
        port      = ports !! nodeIndex
        key       = keys !! nodeIndex
        peerPort  = fromIntegral $ ports !! peerIndex
        peerKey   = keys !! peerIndex

    let config = K.defaultConfig
          { K.k               = k
          , K.routingSharingN = rSharing
          , K.pingTime        = pingTime
          }

    let logError = putStrLn . ("ERROR: " ++)
    let logInfo = putStrLn . ("INFO: " ++)

    putStrLn $ "peerIndex " ++ show peerIndex
    putStrLn $ "peerPort " ++ show peerPort
    kInstance <- K.createL "127.0.0.1" port (KademliaID key) config logInfo logError
    when (peerPort /= 0) $ do
        putStrLn "Connecting to peer"
        r <- connectToPeer kInstance peerPort peerKey
        when (r /= K.JoinSuccess) $
            putStrLn . ("Connection to peer failed "++) . show $ r

    let state = NodeConfig { ncInstance  = kInstance
                           , ncNodeIndex = nodeIndex
                           , ncPort      = fromIntegral port
                           , ncKey       = KademliaID key
                           , ncBctEdges  = bctEdges
                           }
    commands <- map parseCommand . lines <$> getContents
    _ <- S.runStateT (mapM_ executeCommand commands) state
    K.close kInstance
