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
import Network.TCP.Receive

import           Control.Applicative
import           Control.Exception
import           Crypto.Hash.MD5           (hash)
import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Char8     as BS
import           Data.Monoid
import           Network.Simple.TCP
import           Data.Serialize
import qualified Data.Set                  as S
import           Data.Typeable
import           Data.Word
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
epmdRegister port_num name epmd_host epmd_port =
    bracketOnError (connectSock epmd_host epmd_port) (closeSock . fst) $ \(sock, _) -> do
        let alive_req = AliveRequest port_num NormalNode TCP_IPV4 5 5 (fromIntegral (BS.length name)) name 0 ""
        sendPacket2 sock (encodeAliveRequest alive_req)
        runReceive receiveAliveReply sock >>= \case
            Left _err              -> throw EpmdException
            Right (AliveReply 0 _) -> pure (closeSock sock)
            Right _                -> throw EpmdException

-- | Ask for the node info of a named node. Return either the port-please error
-- code, or the port, highest, and lowest protocol versions of the requested
-- node.
epmdNodeInfo :: HostName    -- ^ Epmd host, e.g. "0.0.0.0"
             -> ServiceName -- ^ Epmd port, e.g. "4369"
             -> ByteString  -- ^ Node name.
             -> IO (Either Word8 (Port, HighestVersion, LowestVersion))
epmdNodeInfo epmd_host epmd_port name =
    bracket (connectSock epmd_host epmd_port) (closeSock . fst) $ \(sock, _) -> do
        sendPacket2 sock (encodePortRequest (PortRequest name))
        runReceive receivePortReply sock >>= \case
            Left _err                                           -> throw EpmdException
            Right (PortReplyFailure code)                       -> pure (Left code)
            Right (PortReplySuccess port _ _ hi_ver lo_ver _ _) -> pure (Right (port, hi_ver, lo_ver))

-- | Get all registered names/ports.
epmdNames :: HostName    -- ^ Epmd host, e.g. "0.0.0.0"
          -> ServiceName -- ^ Epmd port, e.g. "4369"
          -> IO [(ByteString, Port)]
epmdNames epmd_host epmd_port =
    bracket (connectSock epmd_host epmd_port) (closeSock . fst) $ \(sock, _) -> do
        sendPacket2 sock encodeNamesRequest
        runReceive receiveNamesReply sock >>= \case
            Left _err                  -> throw EpmdException
            Right (NamesReply _ names) -> pure names

-- -----------------------------------------------------------------------------
-- Handshake API
--

data HandshakeException = HandshakeException String
    deriving (Show, Typeable)

instance Exception HandshakeException

-- | The result of a handshake.
data HandshakeResult
    = HandshakeOk Socket  -- ^ Handshake successful.
    | HandshakeNotOk      -- ^ Handshake already initiated by other node.
    | HandshakeNotAllowed -- ^ Handshake not successful.
    | HandshakeAlive      -- ^ Handshake already succeeded.
    deriving Show

nodeHandshake :: Node -> OutCookie -> InCookie -> Version -> HostName -> ServiceName -> IO HandshakeResult
nodeHandshake node (OutCookie out_cookie) (InCookie in_cookie) version host service =
    bracketOnError (connectSock host service) (closeSock . fst) (nodeHandshake' . fst)
  where
    nodeHandshake' :: Socket -> IO HandshakeResult
    nodeHandshake' sock = send_name (HandshakeName version flags node)
      where
        send_name :: HandshakeName -> IO HandshakeResult
        send_name name = do
            sendPacket2 sock (encodeHandshakeName name)
            recv_status

        recv_status :: IO HandshakeResult
        recv_status =
            recvHsMsg receiveHandshakeStatus sock ("Error receiving status" ++) >>= \case
                HandshakeStatusOk             -> recv_challenge
                HandshakeStatusOkSimultaneous -> recv_challenge
                HandshakeStatusNok            -> pure HandshakeNotOk
                HandshakeStatusNotAllowed     -> pure HandshakeNotAllowed
                HandshakeStatusAlive          -> status_alive
                status                        -> throw (HandshakeException ("Unexpected status: " ++ show status))

        recv_challenge :: IO HandshakeResult
        recv_challenge = do
            HandshakeChallenge _ _ their_challenge _ <-
                recvHsMsg (receiveHandshakeChallenge node) sock ("Error receiving challenge: " ++)
            my_challenge <- generateChallenge
            let digest = generateDigest their_challenge out_cookie
            send_challenge_reply (HandshakeChallengeReply my_challenge digest) my_challenge

        send_challenge_reply :: HandshakeChallengeReply -> Challenge -> IO HandshakeResult
        send_challenge_reply reply challenge = do
            sendPacket2 sock (encodeHandshakeChallengeReply reply)
            recv_challenge_ack challenge

        recv_challenge_ack :: Challenge -> IO HandshakeResult
        recv_challenge_ack my_challenge = do
            let my_digest = generateDigest my_challenge in_cookie

            -- `receiveHandshakeChallengeAck` takes care of comparing digests.
            _ <- recvHsMsg
                   (receiveHandshakeChallengeAck my_digest)
                   sock
                   ("Error receiving challenge ack: " ++)

            pure (HandshakeOk sock)

        status_alive :: IO HandshakeResult
        status_alive = do
            sendPacket2 sock (encodeHandshakeStatus HandshakeStatusFalse)
            pure HandshakeAlive

        flags :: HandshakeFlags
        flags = HandshakeFlags $ S.fromList
            [ FlagExtendedReferences
            , FlagExtendedPidsPorts
            ]


-- Helper function that throws HandshakeExceptions when message parsing fails.
recvHsMsg :: Receive a -> Socket -> (String -> String) -> IO a
recvHsMsg r sock f = runReceive r sock >>= either (throw . HandshakeException . f . show) pure

generateChallenge :: IO Challenge
generateChallenge = randomIO

generateDigest :: Challenge -> ByteString -> Digest
generateDigest (Challenge challenge) cookie = Digest (hash (cookie <> BS.pack (show challenge)))

sendPacket2 :: Socket -> ByteString -> IO ()
sendPacket2 sock bytes = send sock (len <> bytes)
  where
    len :: ByteString
    len = runPut (putWord16be (fromIntegral (BS.length bytes)))
