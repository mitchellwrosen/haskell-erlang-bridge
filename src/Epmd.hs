{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Epmd
    ( epmdNames
    , epmdNodeInfo
    , epmdRegister
    ) where

import Common

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString.Char8            as BS
import           Data.Serialize
import           Data.Typeable
import           Data.Word
import           Network.Simple.TCP

-- -----------------------------------------------------------------------------
-- EPMD API

-- TODO: Expand this type.
data EpmdException = EpmdException
    deriving (Show, Typeable)

instance Exception EpmdException

-- | Register self with EPMD. Returns an action that, when executed,
-- unregisters. Throws an asynchronous exception if the connection fails, or if
-- the register fails.
epmdRegister :: Port -> ByteString -> IO (IO ())
epmdRegister port_num name = do
    (sock, _) <- connectSock "0.0.0.0" "4369"
    let alive_req = AliveRequest port_num NormalNode TCP_IPV4 5 5 (fromIntegral (BS.length name)) name 0 ""
    send sock (encodeWith putAliveRequest alive_req)
    recvEpmdMsg sock getAliveReply >>= \case
        AliveReply result creation
            | result == 0 -> return (closeSock sock)
            | otherwise   ->  closeSock sock >> throw EpmdException

-- | Ask for the node info of a named node. Return either the port-please error
-- code, or the port, highest, and lowest protocol versions of the requested
-- node.
epmdNodeInfo :: ByteString -> IO (Either Word8 (Port, HighestVersion, LowestVersion))
epmdNodeInfo name = do
    (sock, _) <- connectSock "0.0.0.0" "4369"
    send sock (encodeWith putPortRequest (PortRequest name))
    recvEpmdMsg sock getPortReply >>= \case
        PortReplyFailure code -> pure (Left code)
        PortReplySuccess port _ _ highest_ver lowest_ver _ _ _ _ ->
            pure (Right (port, highest_ver, lowest_ver))

-- | Get all registered names/ports.
epmdNames :: IO [(ByteString, Port)]
epmdNames = do
    (sock, _) <- connectSock "0.0.0.0" "4369"
    send sock (runPut putNamesRequest)
    names_reply_names <$> recvEpmdMsg sock getNamesReply

-- Receive a message from epmd. Throw an async exception if the socket is closed
-- or the message doesn't parse.
recvEpmdMsg :: Socket -> Get a -> IO a
recvEpmdMsg sock g =
    (runGet g <$> recvAll sock) >>= \case
        Left _       -> throw EpmdException
        Right result -> return result

-- -----------------------------------------------------------------------------
-- Alive request

data AliveRequest =
    AliveRequest
        Port NodeType Protocol HighestVersion LowestVersion Word16 ByteString Word16 ByteString
    deriving Show

putAliveRequest :: AliveRequest -> Put
putAliveRequest (AliveRequest (Port port)
                              node_type
                              protocol
                              (HighestVersion highest_ver)
                              (LowestVersion lowest_ver)
                              name_len
                              name
                              extra_len
                              extra) = do
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

getAliveReply :: Get AliveReply
getAliveReply = do
    getCode 121
    AliveReply <$> get <*> get

-- -----------------------------------------------------------------------------
-- Port request

data PortRequest = PortRequest ByteString

putPortRequest :: PortRequest -> Put
putPortRequest (PortRequest name) = do
    putWord16be (1 + fromIntegral (BS.length name))
    putWord8 122
    putByteString name

-- -----------------------------------------------------------------------------
-- Port reply

data PortReply
    = PortReplySuccess Port NodeType Protocol HighestVersion LowestVersion Word16 ByteString Word16 ByteString
    | PortReplyFailure Word8

getPortReply :: Get PortReply
getPortReply = do
    getCode 119
    getWord8 >>= \case
        0 -> do
            port_num    <- get
            node_type   <- get
            protocol    <- get
            highest_ver <- get
            lowest_ver  <- get
            name_len    <- get
            name        <- getByteString (fromIntegral name_len)
            extra_len   <- get
            extra       <- getByteString (fromIntegral extra_len)
            pure $ PortReplySuccess (Port port_num)
                                    node_type
                                    protocol
                                    (HighestVersion highest_ver)
                                    (LowestVersion lowest_ver)
                                    name_len
                                    name
                                    extra_len
                                    extra
        n ->
            pure (PortReplyFailure n)

-- -----------------------------------------------------------------------------
-- Names request

putNamesRequest :: Put
putNamesRequest = putWord16be 1 >> putWord8 110

-- -----------------------------------------------------------------------------
-- Names reply

data NamesReply = NamesReply
    { names_reply_epmd_port :: Word32
    , names_reply_names     :: [(ByteString, Port)]
    }

getNamesReply :: Get NamesReply
getNamesReply = do
    port <- getWord32be
    rest <- getRemainingByteString
    case parseOnly (many nameAndPort) rest of
        Left err -> fail err
        Right names_and_ports -> pure (NamesReply port names_and_ports)

-- What the hell, epmd?
-- Parses: "name foo at port 8888"
nameAndPort :: Parser (ByteString, Port)
nameAndPort = do
    string "name "
    name <- takeTill isSpace
    string " at port "
    port <- read <$> many1 digit
    endOfLine
    pure (name, Port port)

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
    deriving Show

instance Serialize Protocol where
    put TCP_IPV4 = putWord8 0

    get = getWord8 >>= \case
        0 -> pure TCP_IPV4
        n -> fail ("Unknown protocol " ++ show n)

newtype HighestVersion = HighestVersion Word16 deriving (Num, Show, Serialize)
newtype LowestVersion  = LowestVersion  Word16 deriving (Num, Show, Serialize)
newtype Port           = Port           Word16 deriving (Num, Show, Serialize)

------------------

encodeWith :: (a -> Put) -> a -> ByteString
encodeWith p = runPut . p

getCode :: Word8 -> Get ()
getCode expected = do
    actual <- getWord8
    when (expected /= actual) $
        fail ("Unexpected code " ++ show actual)
