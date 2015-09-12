module Erlang.Distribution.Term where

import Data.ByteString (ByteString)
import Data.Text       (Text)
import Data.Word

data Term
    = TAtomCacheRef Word8
    | TSmallInteger Word8
    | TInteger Word32
    | TFloat ByteString
    | TAtom Text -- Latin-1 encoded
    | TReference AtomTerm Word32 Word8
    | TPort AtomTerm Word32 Word8
    | TPid AtomTerm Word32 Word32 Word8
    | TSmallTuple [Term]
    | TLargeTuple [Term]
    | TMap [(Term, Term)]
    | TNil
    | TString ByteString
    | TList [Term]
    | TBinary ByteString
    | TSmallBig -- TODO
    | TLargeBig -- TODO
    | TNewReference AtomTerm Word8 ByteString
    | TSmallAtom Text -- Latin-1 encoded
    | TFun PidTerm AtomTerm IntTerm IntTerm [Term]
    | TNewFun
        Word8      -- arity
        ByteString -- "16 bytes MD5 of the significant part of the Beam file" (WTF)
        Word32     -- index
        AtomTerm   -- module
        IntTerm    -- old index
        IntTerm    -- old uniq
        PidTerm    -- pid
        [Term]     -- free variables
    | TExport
        AtomTerm -- M
        AtomTerm -- F
        Word8    -- A
    | TBitBinary -- TODO
    | TNewFloat -- TODO
    | TAtomUtf8 -- TODO
    | TSmallAtomUtf8 -- TODO


data AtomTerm
    = AtomAtom Text
    | AtomSmallAtom Text

data PidTerm
    = PidPid AtomTerm Word32 Word32 Word8

data IntTerm
    = IntSmallInteger Word8
    | IntInteger Word32
