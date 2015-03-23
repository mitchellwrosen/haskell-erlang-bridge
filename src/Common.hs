{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Common where

import           Control.Monad.IO.Class (MonadIO)
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as BS
import           Data.Monoid
import           Data.Serialize
import           Data.Word
import           Network.Simple.TCP

newtype Version = Version Word16 deriving (Num, Show, Serialize)

getRemainingByteString :: Get ByteString
getRemainingByteString = remaining >>= getByteString

-- Recv from a socket until no bytes remain.
recvAll :: forall m. MonadIO m => Socket -> m ByteString
recvAll = go ""
  where
    go :: ByteString -> Socket -> m ByteString
    go acc sock =
        recv sock block_size >>= \case
            Nothing    -> return acc
            Just bytes
                | BS.length bytes == block_size -> go (acc <> bytes) sock
                | otherwise                     -> return (acc <> bytes)

    block_size :: Int
    block_size = 4096
