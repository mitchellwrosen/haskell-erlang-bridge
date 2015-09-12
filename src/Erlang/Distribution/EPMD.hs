module Erlang.Distribution.EPMD
    ( withEpmd
    , EpmdConnection
    , EpmdException(..)
    , epmdNodeInfo
    , epmdNames
    ) where

import Erlang.Distribution.Internal
import Network.TCP.Receive

import           Control.Applicative
import           Control.Exception
import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString.Char8            as BS
import           Data.Serialize
import           Data.Typeable
import           Data.Word
import           Network.Simple.TCP               (Socket)
import qualified Network.Simple.TCP               as TCP

-- -----------------------------------------------------------------------------
-- EPMD API

-- TODO: Expand this type.
data EpmdException = EpmdException String
    deriving (Show, Typeable)

instance Exception EpmdException

newtype EpmdConnection = EC Socket

-- | Register self with EPMD as a normal (visible) node.
withEpmd :: Port                     -- ^ This node's listening port.
         -> ByteString               -- ^ This node's name (must include an @\@@)
         -> TCP.HostName             -- ^ Epmd host, e.g. @localhost@
         -> TCP.ServiceName          -- ^ Epmd port, e.g. @4369@
         -> (EpmdConnection -> IO r) -- ^ Action to perform with the Epmd connection.
         -> IO r
withEpmd port_num name epmd_host epmd_port action =
    TCP.connect epmd_host epmd_port $ \(sock, _) -> do
        let alive_req = AliveRequest port_num NormalNode TCP_IPV4 5 5 (fromIntegral (BS.length name)) name 0 ""
        TCP.send sock (encodeAliveRequest alive_req)
        runReceive receiveAliveReply sock >>= \case
            Left err               -> throw (EpmdException (show err))
            Right (AliveReply 0 _) -> action (EC sock)
            Right _                -> throw (EpmdException "todo")

-- | Ask for the node info of a named node. Return either the port-please error
-- code, or the port, highest, and lowest protocol versions of the requested
-- node.
epmdNodeInfo :: EpmdConnection -- ^ Epmd connection.
             -> ByteString     -- ^ Node name.
             -> IO (Either Word8 (Port, HighestVersion, LowestVersion))
epmdNodeInfo (EC sock) name = do
    TCP.send sock (encodePortRequest (PortRequest name))
    runReceive receivePortReply sock >>= \case
        Left err                                            -> throw (EpmdException (show err))
        Right (PortReplyFailure code)                       -> pure (Left code)
        Right (PortReplySuccess port _ _ hi_ver lo_ver _ _) -> pure (Right (port, hi_ver, lo_ver))

-- | Get all registered names/ports.
epmdNames :: EpmdConnection
          -> IO [(ByteString, Port)]
epmdNames (EC sock) = do
    TCP.send sock encodeNamesRequest
    runReceive receiveNamesReply sock >>= \case
        Left err                   -> throw (EpmdException (show err))
        Right (NamesReply _ names) -> pure names

-- -----------------------------------------------------------------------------
-- Alive request

data AliveRequest =
    AliveRequest
        Port NodeType Protocol HighestVersion LowestVersion Word16 ByteString Word16 ByteString
    deriving Show

encodeAliveRequest :: AliveRequest -> ByteString
encodeAliveRequest (AliveRequest (Port port)
                                 node_type
                                 protocol
                                 (HighestVersion highest_ver)
                                 (LowestVersion lowest_ver)
                                 name_len
                                 name
                                 extra_len
                                 extra) = runPut $ do
        putWord16be (13 + name_len + extra_len)
        putWord8 120
        put port
        put node_type
        put protocol
        put highest_ver
        put lowest_ver
        put name_len
        putByteString name
        put extra_len
        putByteString extra

-- -----------------------------------------------------------------------------
-- Alive reply

data AliveReply = AliveReply Word8 Word16
    deriving Show

receiveAliveReply :: Receive AliveReply
receiveAliveReply = receive 4 `with` p
  where
    p :: Get AliveReply
    p = do
        getCode 121
        AliveReply <$> getWord8 <*> getWord16be

-- -----------------------------------------------------------------------------
-- Port request

data PortRequest = PortRequest ByteString

encodePortRequest :: PortRequest -> ByteString
encodePortRequest (PortRequest name) = runPut $ do
    putWord16be (1 + fromIntegral (BS.length name))
    putWord8 122
    putByteString name

-- -----------------------------------------------------------------------------
-- Port reply

data PortReply
    = PortReplySuccess Port NodeType Protocol HighestVersion LowestVersion ByteString ByteString
    | PortReplyFailure Word8
    deriving Show

receivePortReply :: Receive PortReply
receivePortReply = do
    result <- receive 2 `with` (getCode 119 >> getWord8)
    case result of
        0 -> do
            (a, b, c, d, e) <- receive 8 `with` ((,,,,) <$> get <*> get <*> get <*> get <*> get)
            f <- receivePacket2
            g <- receivePacket2
            pure (PortReplySuccess a b c d e f g)

        n -> pure (PortReplyFailure n)

-- -----------------------------------------------------------------------------
-- Names request

encodeNamesRequest :: ByteString
encodeNamesRequest = runPut $ do
    putWord16be 1
    putWord8 110

-- -----------------------------------------------------------------------------
-- Names reply

data NamesReply = NamesReply Word32 [(ByteString, Port)]

receiveNamesReply :: Receive NamesReply
receiveNamesReply = receiveAll `with` p
  where
    p :: Get NamesReply
    p = do
        port <- getWord32be
        rest <- getBytes =<< remaining
        case parseOnly (many nameAndPort) rest of
            Left err              -> fail err
            Right names_and_ports -> pure (NamesReply port names_and_ports)

    -- What the hell, epmd?
    -- Parses: "name foo at port 8888"
    nameAndPort :: Parser (ByteString, Port)
    nameAndPort = do
        _    <- string "name "
        name <- takeTill isSpace
        _    <- string " at port "
        port <- read <$> many1 digit
        endOfLine
        pure (name, Port port)

--------------------------------------------------------------------------------
-- Misc. types

data NodeType
    = NormalNode
    | HiddenNode
    deriving Show

instance Serialize NodeType where
    put NormalNode = putWord8 77
    put HiddenNode = putWord8 72

    get = getWord8 >>= \case
        77 -> pure NormalNode
        72 -> pure HiddenNode
        n  -> fail ("Unknown node type " ++ show n)

data Protocol = TCP_IPV4
    deriving (Eq, Show)

instance Serialize Protocol where
    put TCP_IPV4 = putWord8 0

    get = getWord8 >>= \case
        0 -> pure TCP_IPV4
        n -> fail ("Unknown protocol " ++ show n)

newtype HighestVersion = HighestVersion Word16 deriving (Num, Show, Serialize)
newtype LowestVersion  = LowestVersion  Word16 deriving (Num, Show, Serialize)
newtype Port           = Port           Word16 deriving (Num, Show, Serialize)
