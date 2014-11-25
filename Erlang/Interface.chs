{-# LANGUAGE LambdaCase #-}

module Erlang.Interface
    ( Buffer
    , Cookie
    , Fd
    , Node
    , NodeName
    , ProcessName
    , connect_init
    , connect
    , erl_init
    , reg_send
    , x_encode_atom
    , x_new_with_version
    -- Higher-level API
    , regSendAtom
    ) where

import Control.Applicative
import Data.ByteString
import Data.Int             (Int16)
import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Storable
import System.IO.Unsafe     (unsafePerformIO)

#include "ei.h"
#include "erl_interface.h"

data Node = Node !(ForeignPtr ())
  deriving Show

data Buffer = Buffer !(ForeignPtr ())

type NodeName    = ByteString
type ProcessName = ByteString
type Cookie      = ByteString

newtype Fd = Fd CInt
  deriving Show

-- erl_init
erl_init :: IO ()
erl_init = {#call unsafe erl_init as c_erl_init#} nullPtr 0

-- ei_connect_init
connect_init :: NodeName -> Cookie -> Int16 -> IO (Either Int Node)
connect_init node_name cookie n =
    useAsCString node_name $ \node_name_cstr ->
    useAsCString cookie    $ \cookie_cstr -> do
        node_ptr <- mallocForeignPtrBytes {#sizeof ei_cnode#}
        withForeignPtr node_ptr $ \raw_node_ptr ->
            {#call unsafe ei_connect_init#}
                   raw_node_ptr
                   node_name_cstr
                   cookie_cstr
                   (CShort n) >>= \case
                0 -> return (Right (Node node_ptr))
                x -> return (Left (fromIntegral x))

-- ei_connect
connect :: Node -> NodeName -> IO (Either Int Fd)
connect (Node node_ptr) node_name =
    useAsCString node_name $ \node_name_cstr ->
        withForeignPtr node_ptr $ \raw_node_ptr -> do
            fd <- {#call unsafe ei_connect#} raw_node_ptr node_name_cstr
            if fd < 0
                then return (Left (fromIntegral fd))
                else return (Right (Fd fd))

-- ei_with_new_version
x_new_with_version :: IO (Maybe Buffer)
x_new_with_version = do
    buf_ptr <- mallocForeignPtrBytes {#sizeof ei_x_buff#}
    withForeignPtr buf_ptr $ \raw_buf_ptr ->
        {#call unsafe ei_x_new_with_version#} raw_buf_ptr >>= \case
            0 -> return (Just (Buffer buf_ptr))
            _ -> return Nothing

-- ei_x_encode_atom. True indicates success; modifies Buffer in-place.
x_encode_atom :: Buffer -> ByteString -> IO Bool
x_encode_atom (Buffer buf_ptr) atom = 
    withForeignPtr buf_ptr $ \raw_buf_ptr ->
        x_encode_atom' raw_buf_ptr atom

-- like x_encode_atom, but takes a raw pointer (unexported).
x_encode_atom' :: Ptr () -> ByteString -> IO Bool
x_encode_atom' raw_ptr atom =
    useAsCString atom $ \atom_cstr ->
        {#call unsafe ei_x_encode_atom#} raw_ptr atom_cstr >>= \case
            0 -> return True
            _ -> return False

-- ei_reg_send
reg_send :: Node -> Fd -> ProcessName -> Buffer -> IO Bool
reg_send node fd proc_name (Buffer buf_ptr) =
    withForeignPtr buf_ptr (reg_send' node fd proc_name)

-- like reg_send, but with a raw pointer (unexported)
reg_send' :: Node -> Fd -> ProcessName -> Ptr () -> IO Bool
reg_send' (Node node_ptr) (Fd fd) proc_name buf_ptr =
    withForeignPtr node_ptr $ \raw_node_ptr ->
    useAsCString proc_name $ \proc_name_cstr -> do
        buf   <- {#get ei_x_buff->buff#}  buf_ptr
        index <- {#get ei_x_buff->index#} buf_ptr
        {#call unsafe ei_reg_send#} 
          raw_node_ptr 
          fd 
          proc_name_cstr 
          buf
          index >>= \case
            0 -> return True
            _ -> return False

--------------------------------------------------------------------------------
-- Higher-level API

regSendAtom :: Node -> Fd -> ProcessName -> ByteString -> IO Bool
regSendAtom node fd proc_name atom =
    allocaBytes {#sizeof ei_x_buff#} $ \buf_ptr ->
        {#call unsafe ei_x_new_with_version#} buf_ptr >>= \case
            0 -> do
                success <- x_encode_atom' buf_ptr atom
                if success
                    then reg_send' node fd proc_name buf_ptr
                    else return False
            _ -> return False
