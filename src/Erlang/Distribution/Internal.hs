module Erlang.Distribution.Internal where

import Prelude hiding (getChar)

import Network.TCP.Receive

import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec.ByteString.Char8
import           Data.Bits
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString.Char8            as BS
import qualified Data.Foldable                    as F
import           Data.Maybe
import           Data.Monoid
import           Data.Serialize
import           Data.Set                         (Set)
import qualified Data.Set                         as S
import           Data.Word
import           GHC.Exts                         (IsString)
import           GHC.Generics
import           System.Random

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

getChar :: Char -> Get ()
getChar expected = do
    actual <- get
    when (expected /= actual) $
        fail ("Unexpected char " ++ [actual])

getCode :: Word8 -> Get ()
getCode expected = do
    actual <- getWord8
    when (expected /= actual) $
        fail ("Unexpected code " ++ show actual)

-- -----------------------------------------------------------------------------
-- Handshake name

data HandshakeName = HandshakeName Version HandshakeFlags Node
    deriving (Eq, Show, Generic)

encodeHandshakeName :: HandshakeName -> ByteString
encodeHandshakeName (HandshakeName version flags (Node name)) = runPut $ do
    putWord16be (fromIntegral (7 + BS.length name))
    put 'n'
    put version
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
        pure (HandshakeName ver flags (Node node))

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
        putWord16be (fromIntegral (1 + BS.length status))
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

data HandshakeChallenge = HandshakeChallenge Version HandshakeFlags Challenge Node
    deriving (Eq, Show, Generic)

encodeHandshakeChallenge :: HandshakeChallenge -> ByteString
encodeHandshakeChallenge (HandshakeChallenge version flags challenge (Node name)) = runPut $ do
    putWord16be (fromIntegral (11 + BS.length name))
    put 'n'
    put version
    put flags
    put challenge
    put name

receiveHandshakeChallenge :: Node -> Receive HandshakeChallenge
receiveHandshakeChallenge (Node expected_node) = receivePacket2 `with` p
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

        pure (HandshakeChallenge ver flags challenge (Node actual_node))

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

newtype Node = Node ByteString
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

data Message = Message DistributionHeader ControlMessage DataMessage
  -- deriving (Eq, Show)

instance Serialize Message where
    put (Message header control_msg data_msg) = do
        let payload = runPut $ do
                          put header
                          put control_msg
                          put data_msg
        putWord32be (fromIntegral (BS.length payload))
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
        putWord8 (fromIntegral (length cache_refs))
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
        putRef (AtomCacheRef _ _ _ seg_index Nothing) = putWord8 seg_index
        putRef (AtomCacheRef _ _ _ seg_index (Just atom_text)) = do
            putWord8 seg_index
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
            seg_index <- getWord8
            len       <- getAtomTextLength is_long_atoms
            atom_text <- getByteString len
            pure (AtomCacheRef s1 s2 s3 seg_index (Just atom_text))
        getRef _ (Nibble _ s1 s2 s3) = do
            seg_index <- getWord8
            pure (AtomCacheRef s1 s2 s3 seg_index Nothing)

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
                        Bool               -- 3 bits of segment index
                        Bool
                        Bool
                        Word8              -- internal segment index
                        (Maybe ByteString) -- Just if new cache entry, Nothing if old
  deriving (Eq, Show)

-- +-------------------+---------------+
-- | 1 bit             | 3 bits        |
-- +-------------------+---------------+
-- | NewCacheEntryFlag | Segment index |
-- +-------------------+---------------+
atomCacheRefFlag :: AtomCacheRef -> Nibble
atomCacheRefFlag (AtomCacheRef b2 b1 b0 _ m) = Nibble (isJust m) b2 b1 b0

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
