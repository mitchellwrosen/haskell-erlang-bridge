{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Handshake
    ( HandshakeResult(..)
    , Node(..)
    , OutCookie(..)
    , InCookie(..)
    , Version(..)
    , handshake
    ) where

import Prelude hiding (getChar)

import Common

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Crypto.Hash.MD5       (hash)
import           Data.Bits
import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.Monoid
import           Data.Serialize
import           Data.Typeable
import           Data.Word
import           GHC.Exts              (IsString)
import           Network.Simple.TCP
import           System.Random

-- -----------------------------------------------------------------------------
-- Handshake API
--

data HandshakeException = HandshakeException ByteString
    deriving (Show, Typeable)

instance Exception HandshakeException

-- | The result of a handshake.
data HandshakeResult
    = HandshakeOk         -- ^ Handshake successful.
    | HandshakeNotOk      -- ^ Handshake already initiated by other node.
    | HandshakeNotAllowed -- ^ Handshake not successful.
    | HandshakeAlive      -- ^ Handshake already succeeded.
    deriving Show

handshake :: Node -> OutCookie -> InCookie -> Version -> HostName -> ServiceName -> IO HandshakeResult
handshake name out_cookie in_cookie version host service = do
    (sock, _) <- connectSock host service
    sendName sock (HandshakeName version flags name) out_cookie in_cookie
  where
    flags :: Flags
    flags = Flags
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

-----

recvHandshakeMsg :: Serialize a => Socket -> Int -> IO a
recvHandshakeMsg sock len = recv sock len >>= \case
    Nothing -> throw (HandshakeException "Socket closed")
    Just reply -> decodeHandshakeReply reply

recvAllHandshakeMsg :: Serialize a => Socket -> IO a
recvAllHandshakeMsg sock = recvAll sock >>= decodeHandshakeReply

decodeHandshakeReply :: Serialize a => ByteString -> IO a
decodeHandshakeReply reply = do
    putStrLn $ "Raw reply: " ++ show reply
    case decode reply of
        Left err     -> throw (HandshakeException ("Decode error: " <> BS.pack err))
        Right result -> return result

generateChallenge :: IO Challenge
generateChallenge = randomIO

generateDigest :: Challenge -> ByteString -> Digest
generateDigest (Challenge challenge) cookie = Digest (hash (cookie <> BS.pack (show challenge)))

-- -----------------------------------------------------------------------------
-- Handshake name

data HandshakeName = HandshakeName Version Flags Node

instance Serialize HandshakeName where
    put (HandshakeName version flags (Node name)) = do
        putWord16be (fromIntegral (7 + BS.length name))
        put 'n'
        put version
        put flags
        putByteString name

    get = do
        len <- getWord16be
        ensure (fromIntegral len)
        getChar 'n'
        HandshakeName <$> get <*> get <*> (Node <$> getByteString (fromIntegral (len - 7)))

-- -----------------------------------------------------------------------------
-- Handshake status

data HandshakeStatus
    = HandshakeStatusOk
    | HandshakeStatusOkSimultaneous
    | HandshakeStatusNok
    | HandshakeStatusNotAllowed
    | HandshakeStatusAlive
    | HandshakeStatusTrue
    | HandshakeStatusFalse
    deriving Show

instance Serialize HandshakeStatus where
    put HandshakeStatusOk             = putStatus "ok"
    put HandshakeStatusOkSimultaneous = putStatus "ok_simultaneous"
    put HandshakeStatusNok            = putStatus "nok"
    put HandshakeStatusNotAllowed     = putStatus "not_allowed"
    put HandshakeStatusAlive          = putStatus "alive"
    put HandshakeStatusTrue           = putStatus "true"
    put HandshakeStatusFalse          = putStatus "false"

    get = do
        getWord16be -- I guess just ignore the length
        getChar 's'
        getRemainingByteString >>= \case
            "ok"              -> pure HandshakeStatusOk
            "ok_simultaneous" -> pure HandshakeStatusOkSimultaneous
            "nok"             -> pure HandshakeStatusNok
            "not_allowed"     -> pure HandshakeStatusNotAllowed
            "alive"           -> pure HandshakeStatusAlive
            "true"            -> pure HandshakeStatusTrue
            "false"           -> pure HandshakeStatusFalse
            x                 -> fail ("Unexpected status " ++ BS.unpack x)

putStatus :: ByteString -> Put
putStatus status = do
    putWord16be (fromIntegral (1 + BS.length status))
    put 's'
    putByteString status

-- -----------------------------------------------------------------------------
-- Handshake challenge

data HandshakeChallenge = HandshakeChallenge Version Flags Challenge ByteString

instance Serialize HandshakeChallenge where
    put (HandshakeChallenge version flags challenge name) = do
        putWord16be (fromIntegral (11 + BS.length name))
        put 'n'
        put version
        put flags
        put challenge
        putByteString name

    get = do
        len <- getWord16be
        ensure (fromIntegral len)
        getChar 'n'
        HandshakeChallenge <$> get <*> get <*> get <*> getByteString (fromIntegral (len - 11))

-- -----------------------------------------------------------------------------
-- Handshake challenge reply

data HandshakeChallengeReply = HandshakeChallengeReply Challenge Digest

instance Serialize HandshakeChallengeReply where
    put (HandshakeChallengeReply challenge digest) = do
        putWord16be 21
        put 'r'
        put challenge
        put digest

    get = do
        len <- getWord16be
        when (len /= fromIntegral 21) $
            fail ("Unexpected challenge reply length " ++ show len)

        getChar 'r'
        HandshakeChallengeReply <$> get <*> get

-- -----------------------------------------------------------------------------
-- Handshake challenge ack

newtype HandshakeChallengeAck = HandshakeChallengeAck Digest

instance Serialize HandshakeChallengeAck where
    put (HandshakeChallengeAck digest) = do
        putWord16be 17
        put 'a'
        put digest

    get = do
        len <- getWord16be
        when (len /= fromIntegral 17) $
            fail ("Unexpected challenge ack length " ++ show len)

        getChar 'a'
        HandshakeChallengeAck <$> get

-- -----------------------------------------------------------------------------
-- Misc types and functions

newtype Node = Node ByteString
    deriving IsString

newtype OutCookie = OutCookie ByteString
    deriving IsString

newtype InCookie = InCookie ByteString
    deriving IsString

newtype Challenge = Challenge Word32
    deriving (Random, Serialize, Show)

newtype Digest = Digest ByteString
    deriving (Eq, Show)

instance Serialize Digest where
    put (Digest digest) = putByteString digest
    get = Digest <$> getByteString 16

newtype Flags = Flags [Flag]

instance Serialize Flags where
    put (Flags xs) = putWord32be (foldr ((.|.) . flagToBits) zeroBits xs)
    get = getWord32be >> pure (Flags []) -- TODO

data Flag
    = FlagPublished
    | FlagAtomCache
    | FlagExtendedReferences
    | FlagDistMonitor
    | FlagFunTags
    | FlagDistMonitorName
    | FlagHiddenAtomCache
    | FlagNewFunTags
    | FlagExtendedPidsPorts
    | FlagExportPtrTag
    | FlagBitBinaries
    | FlagNewFloats
    | FlagUnicodeIO
    | FlagDistHdrAtomCache
    | FlagSmallAtomTags
    | FlagUtf8Atoms

flagToBits :: Flag -> Word32
flagToBits FlagPublished          = bit 0
flagToBits FlagAtomCache          = bit 1
flagToBits FlagExtendedReferences = bit 2
flagToBits FlagDistMonitor        = bit 3
flagToBits FlagFunTags            = bit 4
flagToBits FlagDistMonitorName    = bit 5
flagToBits FlagHiddenAtomCache    = bit 6
flagToBits FlagNewFunTags         = bit 7
flagToBits FlagExtendedPidsPorts  = bit 8
flagToBits FlagExportPtrTag       = bit 9
flagToBits FlagBitBinaries        = bit 10
flagToBits FlagNewFloats          = bit 11
flagToBits FlagUnicodeIO          = bit 12
flagToBits FlagDistHdrAtomCache   = bit 13
flagToBits FlagSmallAtomTags      = bit 14
flagToBits FlagUtf8Atoms          = bit 15

getChar :: Char -> Get ()
getChar expected = do
    actual <- get
    when (expected /= actual) $
        fail ("Unexpected char " ++ [actual])
