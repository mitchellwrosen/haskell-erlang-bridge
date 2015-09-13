module Erlang.Distribution.Term where

import Erlang.Distribution.Internal

import           Control.Monad
import           Data.ByteString        (ByteString)
import           Data.List              (genericLength)
import           Data.Serialize
import           Data.Word

data Term
    = TAtomCacheRef Word8
    | TSmallInteger Word8
    | TInteger Word32
    | TFloat ByteString
    | TAtom ByteString -- Latin-1 encoded
    | TReference
        Term   -- node (atom, small atom, or atom cache ref)
        Word32 -- id
        Word8  -- creation
    | TPort
        Term   -- node (atom, small atom, or atom cache ref)
        Word32 -- id
        Word8  -- creation
    | TPid
        Term   -- node (atom, small atom, or atom cache ref)
        Word32 -- id
        Word32 -- serial
        Word8  -- creation
    | TSmallTuple [Term]
    | TLargeTuple [Term]
    | TMap [(Term, Term)]
    | TNil
    | TString ByteString
    | TList [Term]
    | TBinary ByteString
    | TSmallBig -- TODO
    | TLargeBig -- TODO
    | TNewReference
        Term       -- node (atom, small atom, or atom cache ref)
        Word8      -- creation
        ByteString -- ids (length is a multiple of 4)
    | TSmallAtom ByteString -- Latin-1 encoded
    | TFun
        Term   -- pid
        Term   -- module (atom, small atom, or atom cache ref)
        Term   -- index (small integer or integer)
        Term   -- uniq (small integer or integer)
        [Term] -- free variables
    | TNewFun
        Word8      -- arity
        ByteString -- "16 bytes MD5 of the significant part of the Beam file" (WTF)
        Word32     -- index
        Term       -- module (atom, small atom, or atom cache ref)
        Term       -- old index (small integer or integer)
        Term       -- old uniq (small integer or integer)
        Term       -- pid
        [Term]     -- free variables
    | TExport
        Term -- module (atom, small atom, or atom cache ref)
        Term -- function (atom, small atom, or atom cache ref)
        Term -- arity (small integer)
    | TBitBinary
        Word8      -- num bits of last byte used
        ByteString -- data
    | TNewFloat Double -- 8-byte big-endian IEEE float
    | TAtomUtf8 ByteString -- UTF-8 encoded
    | TSmallAtomUtf8 ByteString -- UTF-8 encoded
    deriving (Eq, Show)

instance Serialize Term where
    put (TAtomCacheRef n) = do
        putWord8 82
        put n
    put (TSmallInteger n) = do
        putWord8 97
        put n
    put (TInteger n) = do
        putWord8 98
        put n
    put (TFloat s) = do
        putWord8 99
        putByteString s
    put (TAtom s) = do
        putWord8 100
        putWord16be (bsLen s)
        putByteString s
    put (TReference node i creation) = do
        ensureAtomTerm node
        putWord8 101
        put node
        put i
        put creation
    put (TPort node i creation) = do
        ensureAtomTerm node
        putWord8 102
        put node
        put i
        put creation
    put (TPid node i serial creation) = do
        ensureAtomTerm node
        putWord8 103
        put node
        put i
        put serial
        put creation
    put (TSmallTuple ts) = do
        putWord8 104
        putWord8 (genericLength ts)
        mapM_ put ts
    put (TLargeTuple ts) = do
        putWord8 105
        putWord32be (genericLength ts)
        mapM_ put ts
    put (TMap kvs) = do
        putWord8 116
        putWord32be (genericLength kvs)
        forM_ kvs $ \(k,v) -> do
            put k
            put v
    put TNil = putWord8 106
    put (TString s) = do
        putWord8 107
        putWord16be (bsLen s)
        putByteString s
    put (TList ts) = do
        putWord8 108
        putWord32be (genericLength ts - 1) -- length does not include last elem
        mapM_ put ts
    put (TBinary b) = do
        putWord8 109
        putWord32be (bsLen b)
        putByteString b
    put TSmallBig = fail "TODO: put TSmallBig"
    put TLargeBig = fail "TODO: put TLargeBig"
    put (TNewReference node creation ids) = do
        ensureAtomTerm node
        putWord8 114
        case bsLen ids `divMod` 4 of
            (n, 0) -> putWord16be n
            (_, _) -> fail "ids field in NEW_REFERENCE_EXT term is not a multiple of 4"
        put node
        put creation
        putByteString ids
    put (TSmallAtom s) = do
        putWord8 115
        putWord8 (bsLen s)
        putByteString s
    put (TFun pid modul index uniq free_vars) = do
        ensurePidTerm pid
        ensureAtomTerm modul
        ensureIntTerm index
        ensureIntTerm uniq
        putWord8 117
        putWord32be (genericLength free_vars)
        put pid
        put modul
        put index
        put uniq
        mapM_ put free_vars
    put (TNewFun arity uniq index modul old_index old_uniq pid free_vars) = do
        ensureAtomTerm modul
        ensureIntTerm old_index
        ensureIntTerm old_uniq
        ensurePidTerm pid
        putWord8 112
        let payload = runPut $ do
                          put arity
                          putByteString uniq
                          put index
                          putWord32be (genericLength free_vars)
                          put modul
                          put old_index
                          put old_uniq
                          put pid
                          mapM_ put free_vars
        putWord32be (4 + bsLen payload)
        putByteString payload
    put (TExport modul fun arity) = do
        ensureAtomTerm modul
        ensureAtomTerm fun
        ensureSmallIntTerm arity
        putWord8 113
        put modul
        put fun
        put arity
    put (TBitBinary bits dat) = do
        putWord8 77
        putWord32be (bsLen dat)
        put bits
        putByteString dat
    put (TNewFloat n) = do
        putWord8 70
        putFloat64be n
    put (TAtomUtf8 s) = do
        putWord8 118
        putWord16be (bsLen s)
        putByteString s
    put (TSmallAtomUtf8 s) = do
        putWord8 119
        putWord8 (bsLen s)
        putByteString s

    -- TODO: ensure during get
    get = getWord8 >>= \case
        82  -> TAtomCacheRef <$> getWord8
        97  -> TSmallInteger <$> getWord8
        98  -> TInteger <$> getWord32be
        99  -> TFloat <$> getByteString 31
        100 -> TAtom <$> getByteStringWith getWord16be
        101 -> TReference <$> getTerm ensureAtomTerm <*> get <*> get
        102 -> TPort <$> getTerm ensureAtomTerm <*> get <*> get
        103 -> TPid <$> getTerm ensureAtomTerm <*> get <*> get <*> get
        104 -> TSmallTuple <$> getListWith getWord8 get
        105 -> TLargeTuple <$> getListWith getWord32be get
        116 -> TMap <$> getListWith getWord32be ((,) <$> get <*> get)
        106 -> pure TNil
        107 -> TString <$> getByteStringWith getWord16be
        108 -> do
            n <- getWord32be
            TList <$> replicateM (fromIntegral n + 1) get -- length does not include last elem
        109 -> TBinary <$> getByteStringWith getWord32be
        110 -> fail "TODO: get TSmallBig"
        111 -> fail "TODO: get TLargeBig"
        114 -> do
            n <- getWord16be
            TNewReference
                <$> getTerm ensureAtomTerm
                <*> get
                <*> getByteString (fromIntegral n * 4)
        115 -> TSmallAtom <$> getByteStringWith getWord8
        117 -> do
            n <- getWord32be
            TFun
                <$> getTerm ensurePidTerm
                <*> getTerm ensureAtomTerm
                <*> getTerm ensureIntTerm
                <*> getTerm ensureIntTerm
                <*> replicateM (fromIntegral n) get
        112 -> do
            _         <- getWord32be
            arity     <- get
            uniq      <- getByteString 16
            index     <- get
            num_free  <- get
            modul     <- getTerm ensureAtomTerm
            old_index <- getTerm ensureIntTerm
            old_uniq  <- getTerm ensureIntTerm
            pid       <- getTerm ensurePidTerm
            free_vars <- replicateM num_free get
            pure $ TNewFun arity uniq index modul old_index old_uniq pid free_vars
        113 -> TExport
            <$> getTerm ensureAtomTerm
            <*> getTerm ensureAtomTerm
            <*> getTerm ensureSmallIntTerm
        77 -> do
            n <- getWord32be
            TBitBinary <$> get <*> getByteString (fromIntegral n)
        70 -> TNewFloat <$> getFloat64be
        118 -> TAtomUtf8 <$> getByteStringWith getWord16be
        119 -> TSmallAtomUtf8 <$> getByteStringWith getWord8
        n  -> fail ("Unknown term type " ++ show n)

-- | Get a Term, and run a check on it.
getTerm :: (Term -> Get ()) -> Get Term
getTerm f = get >>= \t -> do
    f t
    pure t

ensureAtomTerm :: Monad m => Term -> m ()
ensureAtomTerm (TAtom _)         = pure ()
ensureAtomTerm (TSmallAtom _)    = pure ()
ensureAtomTerm (TAtomCacheRef _) = pure ()
ensureAtomTerm t                 = fail $ "\"" ++ show t ++ "\" is not an atom, small atom, or atom cache ref"

ensurePidTerm :: Monad m => Term -> m ()
ensurePidTerm (TPid _ _ _ _) = pure ()
ensurePidTerm t              = fail $ "\"" ++ show t ++ "\" is not a pid"

ensureIntTerm :: Monad m => Term -> m ()
ensureIntTerm (TSmallInteger _) = pure ()
ensureIntTerm (TInteger _)      = pure ()
ensureIntTerm t                 = fail $ "\"" ++ show t ++ "\" is not a small integer or integer"

ensureSmallIntTerm :: Monad m => Term -> m ()
ensureSmallIntTerm (TSmallInteger _) = pure ()
ensureSmallIntTerm t                 = fail $ "\"" ++ show t ++ "\" is not a small integer"
