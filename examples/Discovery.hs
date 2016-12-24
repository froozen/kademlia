import Control.Arrow (first)
import Data.Binary (Binary (..), encode, decodeOrFail,
                   getWord8, putWord8)
import Control.Monad (when)
import Control.Monad.Random (RandomGen, Rand, evalRand, getRandom)
import Control.Monad.Trans (lift)
import qualified Control.Monad.Trans.State as S
import qualified Data.ByteString       as B
import           Data.ByteString.Lazy      (fromStrict, toStrict)
import qualified Data.ByteString.Char8 as C
import System.Random (mkStdGen)
import GHC.Conc (threadDelay)
import qualified Network.Kademlia      as K
import Network (PortNumber)
import System.Environment (getArgs)

data Pong = Pong
          deriving (Eq, Show)

toBSBinary :: Binary b => b -> B.ByteString
toBSBinary = toStrict . encode

fromBSBinary :: Binary b => B.ByteString -> Either String (b, B.ByteString)
fromBSBinary bs =
    case decodeOrFail $ fromStrict bs of
        Left (_, _, errMsg)  -> Left errMsg
        Right (rest, _, res) -> Right (res, toStrict rest)


instance K.Serialize Pong where
  toBS = toBSBinary
  fromBS = fromBSBinary

type KademliaValue = Pong
newtype KademliaID = KademliaID B.ByteString
                   deriving (Show, Eq, Ord)

type KademliaInstance = K.KademliaInstance KademliaID KademliaValue

kIdLength :: Integral a => a
kIdLength = 20

kPrefixLength :: Integral a => a
kPrefixLength = 17

n1 :: Integral a => a
n1 = 10

n2 :: Integral a => a
n2 = 80

t :: Integral a => a
t = 120

randomSeed :: Integral a => a
randomSeed = 42

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

data NodeState = NodeState
    { nsInstance :: KademliaInstance
    , nsNodeIndex :: Int
    }

type NodeMode r = S.StateT NodeState IO r

generateByteString :: (RandomGen g) => Int -> Rand g B.ByteString
generateByteString len = C.pack <$> sequence (replicate len getRandom)

generatePrefix :: (RandomGen g) => Rand g B.ByteString
generatePrefix = generateByteString kPrefixLength

generateKey :: (RandomGen g) => B.ByteString -> Rand g B.ByteString
generateKey prefix = B.append prefix <$> generateByteString (kIdLength - B.length prefix)

generateKeys :: (RandomGen g) => Rand g [B.ByteString]
generateKeys = do
  n1keys <- generate n1 (C.pack "")
  prefix <- generatePrefix
  n2keys <- generate n2 prefix
  return $ n1keys ++ n2keys
  where
    generate count prefix = sequence $ replicate count $ generateKey prefix

generatePorts :: [Int]
generatePorts = [3000 .. 3000 + n1 + n2]

executeCommand :: String -> NodeMode ()
executeCommand "sleep" = do
  lift $ putStrLn "Executing sleep command"
  lift $ threadDelay $ t * 1000000
executeCommand "dump" = do
  lift $ putStrLn "Executing dump command"
  inst <- nsInstance <$> S.get
  lift $ print . length =<< K.dumpPeers inst
  lift $ mapM_ (putStrLn . show) =<< K.dumpPeers inst
executeCommand _ = return ()

connectToPeer :: KademliaInstance -> PortNumber -> B.ByteString -> IO K.JoinResult
connectToPeer inst peerPort peerId = do
    let peerNode = K.Node (K.Peer "127.0.0.1" peerPort) . KademliaID $ peerId
    K.joinNetwork inst peerNode

scenario :: NodeMode ()
scenario = do
  executeCommand "sleep"
  executeCommand "dump"
  executeCommand "sleep"
  executeCommand "sleep"

main :: IO ()
main = do
    [arg0, arg1] <- getArgs
    let nodeIndex = read arg0
        peerIndex = read arg1
        ports = generatePorts
        keys = evalRand generateKeys (mkStdGen randomSeed)
        port = ports !! nodeIndex
        key = keys !! nodeIndex
        peerPort = fromIntegral $ ports !! peerIndex
        peerKey = keys !! peerIndex

    kInstance <- K.create port . KademliaID $ key
    when (peerPort /= 0) $ do
      putStrLn "Connecting to peer"
      r <- connectToPeer kInstance peerPort peerKey
      when (r /= K.JoinSuccess) $
        putStrLn . ("Connection to peer failed "++) . show $ r

    let state = NodeState { nsInstance = kInstance
                          , nsNodeIndex = nodeIndex
                          }
    _ <- S.runStateT scenario state
    K.close kInstance
