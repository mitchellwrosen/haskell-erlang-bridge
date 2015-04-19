module Network.TCP.Receive
    (
    -- * Receive monad
      Receive
    , ReceiveT
    , ReceiveError
    , runReceive
    -- * API
    , receive
    , receiveAll
    , receiveError
    , with
    -- * Derived API
    , receiveWord8
    , receiveSomeWord8
    , receiveSomeWord16be
    , receiveSomeWord32be
    , receivePacket2
    , receivePacket4
    ) where

import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Maybe
import           Data.Monoid
import qualified Data.ByteString           as BS
import           Data.ByteString           (ByteString)
import           Data.Serialize
import           Data.Word
import           Network.Simple.TCP

newtype ReceiveT m a = ReceiveT { runReceiveT :: ReaderT Socket (EitherT ReceiveError m) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Socket)

instance MonadTrans ReceiveT where
    lift = ReceiveT . lift . lift

type Receive = ReceiveT IO

newtype ReceiveError = ReceiveError String

instance Show ReceiveError where
    show (ReceiveError s) = s

runReceive :: Monad m => ReceiveT m a -> Socket -> m (Either ReceiveError a)
runReceive r s = runEitherT (runReaderT (runReceiveT r) s)

receive :: Int -> Receive ByteString
receive n = do
    s <- ask
    liftIO (recvN s n) >>= maybe (receiveError "insufficient bytes") pure

receiveAll :: Receive ByteString
receiveAll = ask >>= recvAll

receiveError :: String -> Receive a
receiveError = ReceiveT . lift . left . ReceiveError

with :: Receive ByteString -> Get a -> Receive a
with r p = r >>= go . runGetPartial p
  where
    go :: Result a -> Receive a
    go (Fail reason _) = receiveError reason
    go (Partial _)     = receiveError "insufficient bytes"
    go (Done a "")     = pure a
    go (Done _ _)      = receiveError "trailing input"

-- -----------------------------------------------------------------------------
-- Derived API

receiveWord8 :: Word8 -> Receive ()
receiveWord8 expected = receive 1 `with` p
  where
    p :: Get ()
    p = do
        actual <- getWord8
        unless (expected == actual) $
            fail ("Expected Word8 " ++ show expected ++ ", found " ++ show actual)

receiveSomeWord8 :: Receive Word8
receiveSomeWord8 = receive 1 `with` getWord8

receiveSomeWord16be :: Receive Word16
receiveSomeWord16be = receive 2 `with` getWord16be

receiveSomeWord32be :: Receive Word32
receiveSomeWord32be = receive 4 `with` getWord32be

receivePacket2 :: Receive ByteString
receivePacket2 = receiveSomeWord16be >>= receive . fromIntegral

receivePacket4 :: Receive ByteString
receivePacket4 = receiveSomeWord32be >>= receive . fromIntegral

-- -----------------------------------------------------------------------------
-- Helper functions

-- | Receive exactly the specified number of bytes.
recvN :: Socket -> Int -> IO (Maybe ByteString)
recvN s = runMaybeT . go
  where
    go :: Int -> MaybeT IO ByteString
    go n = do
      bytes <- MaybeT (recv s n)
      case n - BS.length bytes of
          0 -> pure bytes
          n' -> (bytes <>) <$> go n'

-- | Receive all remaining bytes on the wire.
recvAll :: forall m. (Applicative m, MonadIO m) => Socket -> m ByteString
recvAll = go ""
  where
    go :: ByteString -> Socket -> m ByteString
    go acc sock = recv sock block_size >>= maybe (pure acc) go'
      where
        go' :: ByteString -> m ByteString
        go' bytes = let all_bytes = acc <> bytes
                    in if BS.length bytes == block_size
                           then go all_bytes sock
                           else pure all_bytes

    block_size :: Int
    block_size = 4096
