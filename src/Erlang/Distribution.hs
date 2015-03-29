module Erlang.Distribution
    -- | EPMD
    ( EpmdException(..)
    , epmdNames
    , epmdNodeInfo
    , epmdRegister
    -- | Node communication
    , HandshakeResult(..)
    , Node(..)
    , OutCookie(..)
    , InCookie(..)
    , Version(..)
    , nodeHandshake
    ) where

import Erlang.Distribution.Internal

import           Control.Applicative
import           Control.Exception
import           Control.Monad.IO.Class (MonadIO)
import           Crypto.Hash.MD5        (hash)
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Char8  as BS
import           Data.Monoid
import           Data.Serialize
import qualified Data.Set               as S
import           Data.Typeable
import           Data.Word
import           Network.Simple.TCP
import           System.Random

-- -----------------------------------------------------------------------------
-- EPMD API

-- TODO: Expand this type.
data EpmdException = EpmdException
    deriving (Show, Typeable)

instance Exception EpmdException

-- | Register self with EPMD as a normal (visible) node. Returns an action that,
-- when executed, unregisters. Throws an asynchronous exception if the connection
-- fails, or if the register fails.
epmdRegister :: Port        -- ^ This node's listening port.
             -> ByteString  -- ^ This node's name (must include an \'@\').
             -> HostName    -- ^ Epmd host, e.g. "0.0.0.0"
             -> ServiceName -- ^ Epmd port, e.g. "4369"
             -> IO (IO ())
epmdRegister port_num name epmd_host epmd_port = do
    (sock, _) <- connectSock epmd_host epmd_port
    let alive_req = AliveRequest port_num NormalNode TCP_IPV4 5 5 (fromIntegral (BS.length name)) name 0 ""
    send sock (encodeWith putAliveRequest alive_req)
    recvEpmdMsg sock getAliveReply >>= \case
        AliveReply result _
            | result == 0 -> return (closeSock sock)
            | otherwise   -> closeSock sock >> throw EpmdException

-- | Ask for the node info of a named node. Return either the port-please error
-- code, or the port, highest, and lowest protocol versions of the requested
-- node.
epmdNodeInfo :: HostName    -- ^ Epmd host, e.g. "0.0.0.0"
             -> ServiceName -- ^ Epmd port, e.g. "4369"
             -> ByteString  -- ^ Node name.
             -> IO (Either Word8 (Port, HighestVersion, LowestVersion))
epmdNodeInfo epmd_host epmd_port name = do
    (sock, _) <- connectSock epmd_host epmd_port
    send sock (encodeWith putPortRequest (PortRequest name))
    recvEpmdMsg sock getPortReply >>= \case
        PortReplyFailure code -> pure (Left code)
        PortReplySuccess port _ _ highest_ver lowest_ver _ _ _ _ ->
            pure (Right (port, highest_ver, lowest_ver))

-- | Get all registered names/ports.
epmdNames :: HostName    -- ^ Epmd host, e.g. "0.0.0.0"
          -> ServiceName -- ^ Epmd port, e.g. "4369"
          -> IO [(ByteString, Port)]
epmdNames epmd_host epmd_port = do
    (sock, _) <- connectSock epmd_host epmd_port
    send sock (runPut putNamesRequest)
    names_reply_names <$> recvEpmdMsg sock getNamesReply

-- Receive a message from epmd. Throw an async exception if the socket is closed
-- or the message doesn't parse.
recvEpmdMsg :: Socket -> Get a -> IO a
recvEpmdMsg sock g =
    (runGet g <$> recvAll sock) >>= \case
        Left _       -> throw EpmdException
        Right result -> return result

encodeWith :: (a -> Put) -> a -> ByteString
encodeWith p = runPut . p

-- -----------------------------------------------------------------------------
-- Handshake API
--

data HandshakeException = HandshakeException String
    deriving (Show, Typeable)

instance Exception HandshakeException

-- | The result of a handshake.
data HandshakeResult
    = HandshakeOk         -- ^ Handshake successful.
    | HandshakeNotOk      -- ^ Handshake already initiated by other node.
    | HandshakeNotAllowed -- ^ Handshake not successful.
    | HandshakeAlive      -- ^ Handshake already succeeded.
    deriving Show

nodeHandshake :: Node -> OutCookie -> InCookie -> Version -> HostName -> ServiceName -> IO HandshakeResult
nodeHandshake name out_cookie in_cookie version host service = do
    (sock, _) <- connectSock host service
    sendName sock (HandshakeName version flags name) out_cookie in_cookie
  where
    flags :: HandshakeFlags
    flags = HandshakeFlags $ S.fromList
        [ FlagExtendedReferences
        , FlagExtendedPidsPorts
        ]

sendName :: Socket -> HandshakeName -> OutCookie -> InCookie -> IO HandshakeResult
sendName sock name out_cookie in_cookie = do
    send sock (encode name)
    recvStatus sock out_cookie in_cookie

recvStatus :: Socket -> OutCookie -> InCookie -> IO HandshakeResult
recvStatus sock out_cookie in_cookie =
    recvAllHandshakeMsg sock >>= \case
        HandshakeStatusOk             -> recvChallenge sock out_cookie in_cookie
        HandshakeStatusOkSimultaneous -> recvChallenge sock out_cookie in_cookie
        HandshakeStatusNok            -> return HandshakeNotOk
        HandshakeStatusNotAllowed     -> return HandshakeNotAllowed
        HandshakeStatusAlive          -> send sock (encode HandshakeStatusFalse) >> return HandshakeAlive
        status                        -> throw (HandshakeException ("Unexpected status: " ++ show status))

recvChallenge :: Socket -> OutCookie -> InCookie -> IO HandshakeResult
recvChallenge sock (OutCookie out_cookie) in_cookie = do
    HandshakeChallenge _ _ their_challenge _ <- recvAllHandshakeMsg sock
    my_challenge <- generateChallenge
    let digest = generateDigest their_challenge out_cookie
    sendChallengeReply sock (HandshakeChallengeReply my_challenge digest) my_challenge in_cookie

sendChallengeReply :: Socket -> HandshakeChallengeReply -> Challenge -> InCookie -> IO HandshakeResult
sendChallengeReply sock reply challenge in_cookie = do
    send sock (encode reply)
    recvChallengeAck sock challenge in_cookie

recvChallengeAck :: Socket -> Challenge -> InCookie -> IO HandshakeResult
recvChallengeAck sock my_challenge (InCookie in_cookie) = do
    HandshakeChallengeAck their_digest <- recvAllHandshakeMsg sock
    let my_digest = generateDigest my_challenge in_cookie
    if my_digest == their_digest
        then return HandshakeOk
        else throw (HandshakeException "Digest mismatch")

recvAllHandshakeMsg :: Serialize a => Socket -> IO a
recvAllHandshakeMsg sock = recvAll sock >>= decodeHandshakeReply

decodeHandshakeReply :: Serialize a => ByteString -> IO a
decodeHandshakeReply reply = do
    putStrLn $ "Raw reply: " ++ show reply
    case decode reply of
        Left err     -> throw (HandshakeException ("Decode error: " ++ err))
        Right result -> return result

generateChallenge :: IO Challenge
generateChallenge = randomIO

generateDigest :: Challenge -> ByteString -> Digest
generateDigest (Challenge challenge) cookie = Digest (hash (cookie <> BS.pack (show challenge)))

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
