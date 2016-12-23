import Control.Arrow (first)
import Data.Binary (Binary (..), encode, decodeOrFail,
                   getWord8, putWord8)
import Control.Monad.Trans (lift)
import qualified Control.Monad.Trans.State as S
import qualified Data.ByteString       as B
import           Data.ByteString.Lazy      (fromStrict, toStrict)
import qualified Data.ByteString.Char8 as C
import GHC.Conc (threadDelay)
import qualified Network.Kademlia      as K

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
kIdLength = 5

instance K.Serialize KademliaID where
   toBS (KademliaID bs)
       | B.length bs >= kIdLength = B.take kIdLength bs
       | otherwise        = error "KademliaID to short!"

   fromBS bs
       | B.length bs >= kIdLength = Right . first KademliaID . B.splitAt kIdLength $ bs
       | otherwise        = Left "ByteString too short!"

instance Binary Pong where
    put _ = putWord8 1
    get = do
        w <- getWord8
        if w == 1
        then pure Pong
        else fail "no parse pong"

data NodeState = NodeState
    { nsInstance :: KademliaInstance
    , nsDelay :: Int
    , nsKeySize :: Int
    , nsKeyPrefix :: B.ByteString
    }

executeCommand :: String -> S.StateT NodeState IO ()
executeCommand "sleep" = do
  delay <- (* 1000000) . nsDelay <$> S.get
  lift $ threadDelay delay
executeCommand "dump" = return ()
executeCommand _ = return ()

main :: IO ()
main = do
    kInstance <- K.create 12345 . KademliaID . C.pack $ "hello" :: IO (K.KademliaInstance KademliaID KademliaValue)
    let state = NodeState { nsInstance = kInstance
                          , nsDelay = 5
                          , nsKeySize = 160
                          , nsKeyPrefix = C.pack "prefix"
                          }
    _ <- S.runStateT (executeCommand "sleep") state
    K.close kInstance
