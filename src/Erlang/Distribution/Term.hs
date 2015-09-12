module Erlang.Distribution.Term where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Serialize
import           Data.Word

data Term
    = TAtomCacheRef Word8
    | TSmallInteger Word8
    | TInteger Word32
    | TFloat ByteString
    | TAtom ByteString
    | TReference ByteString Word32 Word8
    | TPort ByteString Word32 Word8
    | TPid ByteString Word32 Word32 Word8
    | TSmallTuple [ByteString]
    | TLargeTuple [ByteString]
    | TMap [(ByteString, ByteString)]
    | TNil
    | TString ByteString
    | TList [ByteString]
    | TBinary ByteString
    | TSmallBig -- TODO
    | TLargeBig -- TODO
    | TNewReference ByteString Word8 ByteString
    | TSmallAtom ByteString
    | TFun ByteString ByteString ByteString ByteString [ByteString]
    | TNewFun
        Word8        -- arity
        ByteString   -- "16 bytes MD5 of the significant part of the Beam file" (WTF)
        Word32       -- index
        ByteString   -- module
        ByteString   -- old index
        ByteString   -- old uniq
        ByteString   -- pid
        [ByteString] -- free variables
    | TExport
        ByteString -- M
        ByteString -- F
        Word8      -- A
    | TBitBinary -- TODO
    | TNewFloat -- TODO
    | TAtomUtf8 -- TODO
    | TSmallAtomUtf8 -- TODO

instance Serialize Term where
    put (TAtomCacheRef n) = do
        putWord8 82
        putWord8 n
    put (TSmallInteger n) = do
        putWord8 97
        putWord8 n
    put (TInteger n) = do
        putWord8 98
        putWord32be n
    put (TFloat s) = do
        putWord8 99
        putByteString s
    put (TAtom s) = do
        putWord8 100
        putWord16be (fromIntegral (BS.length s))
        putByteString s
    put (TReference node i creation) = do
        putWord8 101
        putByteString node
        putWord32be i
        putWord8 creation
    put (TPort node i creation) = do
        putWord8 102
        putByteString node
        putWord32be i
        putWord8 creation
    put (TPid node i serial creation) = do
        putWord8 103
        putByteString node
        putWord32be i
        putWord32be serial
        putWord8 creation
    put _ = error "TODO"

    get = error "TODO"
