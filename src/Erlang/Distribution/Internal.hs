module Erlang.Distribution.Internal where

import Control.Monad
import Data.Serialize
import Data.Word

getCode :: Word8 -> Get ()
getCode expected = do
    actual <- getWord8
    when (expected /= actual) $
        fail ("Unexpected code " ++ show actual)
