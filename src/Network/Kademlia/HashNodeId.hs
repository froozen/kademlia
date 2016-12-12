module Network.Kademlia.HashNodeId
    ( Nonce
    , genNonce
    , HashId
    , hashIdLength
    , hashAddress
    , verifyAddress
    ) where

import qualified Crypto.KDF.PBKDF2 as PBKDF2
import           Crypto.Hash (SHA256(..), SHA512, Digest, hash)
import           Crypto.Random
import           Data.Word
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import           Network.Kademlia.Types

newtype Nonce = Nonce B.ByteString

newtype HashId = HashId B.ByteString
    deriving (Show,Eq)

instance Serialize HashId where
    toBS (HashId h) = h
    fromBS bs
        | B.length bs >= hashIdLength =
            let (b1, b2) = B.splitAt hashIdLength bs
             in if verifyAddress b1 then Right (HashId b1, b2) else Left "invalid hash id"
        | otherwise                   = Left "invalid size for hashid"

hashIdLength :: Int 
hashIdLength = outputLen + nonceLen

-- Parameters for the hashing function. 500 iter of PBDKF2 with HMAC-SHA256
nonceLen = 14 -- 14 bytes of nonce
outputLen = 18 -- 18 bytes of hashing
iter = 500

hashingFct pass salt = PBKDF2.generate (PBKDF2.prfHMAC SHA256) (PBKDF2.Parameters 500 outputLen) pass salt

nonceToSalt :: Nonce -> Digest SHA512
nonceToSalt (Nonce n) = hash n

genNonce :: MonadRandom randomly => randomly Nonce
genNonce = Nonce <$> getRandomBytes nonceLen

-- | Calculate ID based on addresses
hashAddress :: Nonce -> HashId
hashAddress nonce@(Nonce ad) = HashId $ B.concat [ad, hashingFct ad (nonceToSalt nonce)]

-- | Verify ID
verifyAddress :: B.ByteString -> Bool
verifyAddress bs
    | B.length bs /= hashIdLength = False
    | otherwise                   =
        let (n, _) = B.splitAt nonceLen bs
            hid    = hashAddress (Nonce n)
         in hid == HashId bs
