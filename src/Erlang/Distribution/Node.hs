{-# LANGUAGE ScopedTypeVariables #-}

module Erlang.Distribution.Node
    ( withNode
    , Node
    , HandshakeException(..)
    ) where

import Erlang.Distribution.Internal
import Network.TCP.Receive

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Crypto.Hash.MD5       (hash)
import           Data.Bits
import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Foldable         as F
import           Data.List             (genericLength)
import           Data.Maybe
import           Data.Monoid
import           Data.Serialize
import           Data.Set              (Set)
import qualified Data.Set              as S
import           Data.Typeable
import           Data.Word
import           GHC.Exts              (IsString)
import           GHC.Generics
import           Network.Simple.TCP    (Socket)
import qualified Network.Simple.TCP    as TCP
import           Prelude               hiding (getChar)
import           System.Random

data HandshakeException
    = HandshakeNotOk      -- ^ Handshake already initiated by remote node.
    | HandshakeNotAllowed -- ^ Handshake not successful.
    | HandshakeAlive      -- ^ The connection to the remote node was already active.
    | HandshakeException String
    deriving (Show, Typeable)

instance Exception HandshakeException

newtype Node = Node Socket

withNode :: forall r.
            NodeName        -- ^ Local node's name.
         -> OutCookie       -- ^ Cookie for outgoing communication to remote node.
         -> InCookie        -- ^ Cookie expected to be used for incoming communication from remote node.
         -> Version         -- ^ Distribution version. Hasnt changed since @R6B@, and should always be set to @5@.
         -> TCP.HostName    -- ^ Remote node host.
         -> TCP.ServiceName -- ^ Remote node port.
         -> (Node -> IO r)  -- ^ Action to perform with the node connection.
         -> IO r
withNode node_name (OutCookie out_cookie) (InCookie in_cookie) ver remote_host remote_port action =
    TCP.connect remote_host remote_port (handshake . fst)
  where
    handshake :: Socket -> IO r
    handshake sock = send_name (HandshakeName ver flags node_name)
      where
        send_name :: HandshakeName -> IO r
        send_name name = do
            TCP.send sock (encodeHandshakeName name)
            recv_status

        recv_status :: IO r
        recv_status =
            recvHsMsg receiveHandshakeStatus sock ("Error receiving status" ++) >>= \case
                HandshakeStatusOk             -> recv_challenge
                HandshakeStatusOkSimultaneous -> recv_challenge
                HandshakeStatusNok            -> throw HandshakeNotOk
                HandshakeStatusNotAllowed     -> throw HandshakeNotAllowed
                HandshakeStatusAlive          -> status_alive
                status                        -> throw (HandshakeException ("Unexpected status: " ++ show status))

        recv_challenge :: IO r
        recv_challenge = do
            HandshakeChallenge _ _ their_challenge _ <-
                recvHsMsg (receiveHandshakeChallenge node_name) sock ("Error receiving challenge: " ++)
            my_challenge <- generateChallenge
            let digest = generateDigest their_challenge out_cookie
            send_challenge_reply (HandshakeChallengeReply my_challenge digest) my_challenge

        send_challenge_reply :: HandshakeChallengeReply -> Challenge -> IO r
        send_challenge_reply reply challenge = do
            TCP.send sock (encodeHandshakeChallengeReply reply)
            recv_challenge_ack challenge

        recv_challenge_ack :: Challenge -> IO r
        recv_challenge_ack my_challenge = do
            let my_digest = generateDigest my_challenge in_cookie

            -- `receiveHandshakeChallengeAck` takes care of comparing digests.
            _ <- recvHsMsg
                   (receiveHandshakeChallengeAck my_digest)
                   sock
                   ("Error receiving challenge ack: " ++)

            action (Node sock)

        status_alive :: IO r
        status_alive = do
            TCP.send sock (encodeHandshakeStatus HandshakeStatusFalse)
            throw HandshakeAlive

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

-- -----------------------------------------------------------------------------
-- Handshake name

data HandshakeName = HandshakeName Version HandshakeFlags NodeName
    deriving (Eq, Show, Generic)

encodeHandshakeName :: HandshakeName -> ByteString
encodeHandshakeName (HandshakeName ver flags (NodeName name)) = runPut $ do
    putWord16be (7 + bsLen name)
    put 'n'
    put ver
    put flags
    putByteString name

receiveHandshakeName :: Receive HandshakeName
receiveHandshakeName = receivePacket2 `with` p
  where
    p :: Get HandshakeName
    p = do
        getChar 'n'
        ver <- get
        flags <- get
        node <- getBytes =<< remaining
        pure (HandshakeName ver flags (NodeName node))

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
    deriving (Bounded, Enum, Eq, Show)

encodeHandshakeStatus :: HandshakeStatus -> ByteString
encodeHandshakeStatus = runPut . p
  where
    p :: HandshakeStatus -> Put
    p HandshakeStatusOk             = putStatus "ok"
    p HandshakeStatusOkSimultaneous = putStatus "ok_simultaneous"
    p HandshakeStatusNok            = putStatus "nok"
    p HandshakeStatusNotAllowed     = putStatus "not_allowed"
    p HandshakeStatusAlive          = putStatus "alive"
    p HandshakeStatusTrue           = putStatus "true"
    p HandshakeStatusFalse          = putStatus "false"

    putStatus :: ByteString -> Put
    putStatus status = do
        putWord16be (1 + bsLen status)
        put 's'
        putByteString status

receiveHandshakeStatus :: Receive HandshakeStatus
receiveHandshakeStatus = receivePacket2 `with` p
  where
    p :: Get HandshakeStatus
    p = do
        getChar 's'
        remaining >>= getBytes >>= \case
            "ok"              -> pure HandshakeStatusOk
            "ok_simultaneous" -> pure HandshakeStatusOkSimultaneous
            "nok"             -> pure HandshakeStatusNok
            "not_allowed"     -> pure HandshakeStatusNotAllowed
            "alive"           -> pure HandshakeStatusAlive
            "true"            -> pure HandshakeStatusTrue
            "false"           -> pure HandshakeStatusFalse
            x                 -> fail ("Unexpected status " ++ BS.unpack x)

-- -----------------------------------------------------------------------------
-- Handshake challenge

data HandshakeChallenge = HandshakeChallenge Version HandshakeFlags Challenge NodeName
    deriving (Eq, Show, Generic)

encodeHandshakeChallenge :: HandshakeChallenge -> ByteString
encodeHandshakeChallenge (HandshakeChallenge ver flags challenge (NodeName name)) = runPut $ do
    putWord16be (11 + bsLen name)
    put 'n'
    put ver
    put flags
    put challenge
    put name

receiveHandshakeChallenge :: NodeName -> Receive HandshakeChallenge
receiveHandshakeChallenge (NodeName expected_node) = receivePacket2 `with` p
  where
    p :: Get HandshakeChallenge
    p = do
        getChar 'n'
        ver         <- get
        flags       <- get
        challenge   <- get
        actual_node <- getBytes =<< remaining

        unless (expected_node == actual_node) $
            fail ("In challenge receive, expected node "
                  ++ BS.unpack expected_node
                  ++ " but found node "
                  ++ BS.unpack actual_node)

        pure (HandshakeChallenge ver flags challenge (NodeName actual_node))

-- -----------------------------------------------------------------------------
-- Handshake challenge reply

data HandshakeChallengeReply = HandshakeChallengeReply Challenge Digest
    deriving (Eq, Show, Generic)

encodeHandshakeChallengeReply :: HandshakeChallengeReply -> ByteString
encodeHandshakeChallengeReply (HandshakeChallengeReply challenge digest) = runPut $ do
    putWord16be 21
    put 'r'
    put challenge
    put digest

receiveHandshakeChallengeReply :: Receive HandshakeChallengeReply
receiveHandshakeChallengeReply = receivePacket2 `with` p
  where
    p :: Get HandshakeChallengeReply
    p = do
        getChar 'r'
        HandshakeChallengeReply <$> get <*> get

-- -----------------------------------------------------------------------------
-- Handshake challenge ack

newtype HandshakeChallengeAck = HandshakeChallengeAck Digest
    deriving (Eq, Show)

encodeHandshakeChallengeAck :: HandshakeChallengeAck -> ByteString
encodeHandshakeChallengeAck (HandshakeChallengeAck digest) = runPut $ do
    putWord16be 17
    put 'a'
    put digest

receiveHandshakeChallengeAck :: Digest -> Receive HandshakeChallengeAck
receiveHandshakeChallengeAck (Digest my_digest) = receivePacket2 `with` p
  where
    p :: Get HandshakeChallengeAck
    p = do
        getChar 'a'
        Digest their_digest <- get

        unless (my_digest == their_digest) $
            fail ("Expected digest "
                  ++ BS.unpack my_digest
                  ++ " but found digest "
                  ++ BS.unpack their_digest)

        pure (HandshakeChallengeAck (Digest their_digest))

-- -----------------------------------------------------------------------------
-- Misc types and functions

newtype Version = Version Word16
    deriving (Eq, Num, Show, Serialize)

newtype NodeName = NodeName ByteString
    deriving (Eq, Show, IsString)

newtype OutCookie = OutCookie ByteString
    deriving (Eq, IsString)

newtype InCookie = InCookie ByteString
    deriving (Eq, IsString)

newtype Challenge = Challenge Word32
    deriving (Eq, Show, Random, Serialize)

newtype Digest = Digest ByteString
    deriving (Eq, Show)

instance Serialize Digest where
    put (Digest digest) = putByteString digest
    get = Digest <$> getByteString 16

data HandshakeFlag
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
    deriving (Bounded, Enum, Eq, Ord, Show)

flagToBits :: HandshakeFlag -> Word32
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

newtype HandshakeFlags = HandshakeFlags (Set HandshakeFlag)
    deriving (Eq, Show)

instance Serialize HandshakeFlags where
    put (HandshakeFlags flags) = putWord32be (F.foldr ((.|.) . flagToBits) zeroBits flags)
    get = HandshakeFlags . go <$> getWord32be
      where
        go :: Word32 -> Set HandshakeFlag
        go n =
            --  add_flag :: Int -> HandshakeFlag -> Set HandshakeFlag
            let add_flag i flag = if testBit n i
                                      then S.singleton flag
                                      else mempty
            in mconcat (map (uncurry add_flag) (zip [0..15] [minBound..maxBound]))

-- -----------------------------------------------------------------------------
-- Message

-- Nodes running erts >= 5.7.2 will always send a DistributionHeader.
data Message = Message (Maybe DistributionHeader) ControlMessage DataMessage
  -- deriving (Eq, Show)

instance Serialize Message where
    put (Message mheader control_msg data_msg) = do
        let payload = runPut $ do
                          case mheader of
                              Just header -> put header
                              Nothing     -> putWord8 112
                          put control_msg
                          put data_msg
        putWord32be (bsLen payload)
        putByteString payload

    get = do
        len <- getWord32be
        _   <- ensure (fromIntegral len)
        Message <$> get <*> get <*> get

-- -----------------------------------------------------------------------------
-- Distribution header

data DistributionHeader = DistributionHeader
                              Bool -- is long atoms? (not relevant if no cache refs)
                              [AtomCacheRef]
  deriving (Show, Eq)

instance Serialize DistributionHeader where
    put (DistributionHeader _ []) = do
        putWord8 131
        putWord8 68
        putWord8 0
    put (DistributionHeader is_long_atoms cache_refs) = do
        putWord8 131
        putWord8 68
        putWord8 (genericLength cache_refs)
        putFlags cache_refs
        mapM_ putRef cache_refs
      where
        -- +-------------------+---------------+
        -- | 1 bit             | 3 bits        |
        -- +-------------------+---------------+
        -- | NewCacheEntryFlag | Segment index |
        -- +-------------------+---------------+
        --
        -- per cache ref, followed by
        --
        -- +-----------------+-----------+
        -- | 3 bits          | 1 bit     |
        -- +-----------------+-----------+
        -- | CurrentlyUnused | LongAtoms |
        -- +-----------------+-----------+
        putFlags :: [AtomCacheRef] -> Put
        -- No more flags to put: is_long_atoms takes up a whole byte.
        putFlags []
            | is_long_atoms = putWord8 (bit 4)
            | otherwise     = putWord8 0

        -- Odd number of flags: the last flag goes in the most significant
        -- nibble, and the long atoms goes in the least significant nibble.
        putFlags [cache_ref] = let Nibble b7 b6 b5 b4 = atomCacheRefFlag cache_ref
                               in putWord8 (word8bits b7 b6 b5 b4 False False False is_long_atoms)

        -- Even number of flags: the even-indexed flags go in the least
        -- significant nibble
        putFlags (cache_ref_1 : cache_ref_2 : rest) = do
            let Nibble b3 b2 b1 b0 = atomCacheRefFlag cache_ref_1
                Nibble b7 b6 b5 b4 = atomCacheRefFlag cache_ref_2
            putWord8 (word8bits b7 b6 b5 b4 b3 b2 b1 b0)
            putFlags rest

        -- +----------------------+-------------------+
        -- | 1                    | 1 | 2  | Length   |
        -- +----------------------+--------+----------+
        -- | InternalSegmentIndex | Length | AtomText |
        -- +----------------------+--------+----------+
        --
        -- if new cache entry, otherwise
        --
        -- +----------------------+
        -- | 1                    |
        -- +----------------------+
        -- | InternalSegmentIndex |
        -- +----------------------+
        putRef :: AtomCacheRef -> Put
        putRef (AtomCacheRef _ internal_seg_index Nothing) = putWord8 internal_seg_index
        putRef (AtomCacheRef _ internal_seg_index (Just atom_text)) = do
            putWord8 internal_seg_index
            putAtomTextLength (BS.length (atom_text))
            putByteString atom_text

        -- Put either 1 or 2 bytes of length, per is_long_atoms
        putAtomTextLength :: Int -> Put
        putAtomTextLength n
            | is_long_atoms = putWord16be (fromIntegral n)
            | otherwise     = putWord8    (fromIntegral n)

    get = do
        getCode 131
        getCode 68
        getWord8 >>= \case
            0 -> pure (DistributionHeader False []) -- 'False' here is meaningless
            n -> do
                (flags, is_long_atoms) <- getFlags (fromIntegral n)
                refs <- forM flags (getRef is_long_atoms)
                pure (DistributionHeader is_long_atoms refs)
      where
        -- Get flags and the nibble at the end of the flags (which only
        -- contains one interesting bit)
        getFlags :: Int -> Get ([Nibble], Bool)
        getFlags n | even n = do
            flags         <- splitAndFlipWord8s <$> replicateM (n `div` 2) getWord8
            is_long_atoms <- (`testBit` 4) <$> getWord8
            pure (flags, is_long_atoms)
        getFlags n = do
            flags <- splitAndFlipWord8s <$> replicateM (n `div` 2) getWord8
            (last_flag, Nibble _ _ _ is_long_atoms) <- splitWord8 <$> getWord8
            pure (flags ++ [last_flag], is_long_atoms)

        getRef :: Bool -> Nibble -> Get AtomCacheRef
        getRef is_long_atoms (Nibble True s1 s2 s3) = do
            internal_seg_index <- getWord8
            len                <- getAtomTextLength is_long_atoms
            atom_text          <- getByteString len
            pure (AtomCacheRef (SegmentIndex s1 s2 s3) internal_seg_index (Just atom_text))
        getRef _ (Nibble _ s1 s2 s3) = do
            internal_seg_index <- getWord8
            pure (AtomCacheRef (SegmentIndex s1 s2 s3) internal_seg_index Nothing)

        -- Get either 1 or 2 bytes of length, per is_long_atoms.
        getAtomTextLength :: Bool -> Get Int
        getAtomTextLength True  = fromIntegral <$> getWord16be
        getAtomTextLength False = fromIntegral <$> getWord8

        -- {7 6 5 4 3 2 1 0} -> [{3 2 1 0}, {7 6 5 4}]
        --
        -- This odd function exists because "Flags for an even
        -- AtomCacheReferenceIndex are located in the least significant half
        -- byte and flags for an odd AtomCacheReferenceIndex are located in the
        -- most significant half byte."
        splitAndFlipWord8s :: [Word8] -> [Nibble]
        splitAndFlipWord8s = (>>= go)
          where
            go :: Word8 -> [Nibble]
            go w = [ Nibble (testBit w 3) (testBit w 2) (testBit w 1) (testBit w 0)
                   , Nibble (testBit w 7) (testBit w 6) (testBit w 5) (testBit w 4)
                   ]

        -- {7 6 5 4 3 2 1 0} -> ({7 6 5 4}, {3 2 1 0})
        splitWord8 :: Word8 -> (Nibble, Nibble)
        splitWord8 w = let [n2, n1] = splitAndFlipWord8s [w] in (n1, n2)

word8bits :: Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Word8
word8bits b7 b6 b5 b4 b3 b2 b1 b0 =
    set_bit_if b7 7 .|.
    set_bit_if b6 6 .|.
    set_bit_if b5 5 .|.
    set_bit_if b4 4 .|.
    set_bit_if b3 3 .|.
    set_bit_if b2 2 .|.
    set_bit_if b1 1 .|.
    set_bit_if b0 0
  where
    set_bit_if :: Bool -> Int -> Word8
    set_bit_if b i = if b then bit i else 0

data AtomCacheRef = AtomCacheRef
                        SegmentIndex         -- 3 bits of segment index
                        InternalSegmentIndex -- internal segment index
                        (Maybe ByteString)   -- Just if new cache entry, Nothing if old
  deriving (Eq, Show)

-- +-------------------+---------------+
-- | 1 bit             | 3 bits        |
-- +-------------------+---------------+
-- | NewCacheEntryFlag | Segment index |
-- +-------------------+---------------+
atomCacheRefFlag :: AtomCacheRef -> Nibble
atomCacheRefFlag (AtomCacheRef (SegmentIndex b2 b1 b0) _ m) = Nibble (isJust m) b2 b1 b0

data SegmentIndex = SegmentIndex Bool Bool Bool
  deriving (Eq, Show)

type InternalSegmentIndex = Word8

-- -----------------------------------------------------------------------------
-- Control message

data ControlMessage = ControlMessage
    -- = ControlMessageLink ErlPid ErlPid
    -- | ControlMessageSend ErlBinary ErlPid
    -- TODO: the rest

instance Serialize ControlMessage where
    put = undefined
    get = undefined

-- -----------------------------------------------------------------------------
-- Data message

data DataMessage = DataMessage

instance Serialize DataMessage where
    put = undefined
    get = undefined

data Nibble = Nibble Bool Bool Bool Bool

--------------------------------------------------------------------------------
-- Misc. helpers

getChar :: Char -> Get ()
getChar expected = do
    actual <- get
    when (expected /= actual) $
        fail ("Unexpected char " ++ [actual])
