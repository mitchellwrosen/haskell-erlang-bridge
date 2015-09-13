module Erlang.Distribution.Internal where

import           Control.Monad
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Serialize
import           Data.Word

getCode :: Word8 -> Get ()
getCode expected = do
    actual <- getWord8
    when (expected /= actual) $
        fail ("Unexpected code " ++ show actual)

-- | Generic ByteString length
bsLen :: Num a => ByteString -> a
bsLen = fromIntegral . BS.length

-- | Get a ByteString preceded by its length, given a getter for the length.
getByteStringWith :: Integral a => Get a -> Get ByteString
getByteStringWith len = len >>= getByteString . fromIntegral

-- | Get a list of elements preceded by their length, give a getter for
-- the length and a getter for a single element.
getListWith :: Integral a => Get a -> Get r -> Get [r]
getListWith len x = len >>= \n -> replicateM (fromIntegral n) x
