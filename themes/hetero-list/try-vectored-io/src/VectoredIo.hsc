{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs, DataKinds, TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module VectoredIo (readv, writev) where

import Foreign.Ptr (Ptr)
import Foreign.C.Types (CInt(..))
import Data.Int (Int64)
import System.Posix.Types (Fd(..))

import HeteroList (HeteroPtrList)
import Iovec (Iovec, PrepareIovec, withIovec)

#include <sys/uio.h>

foreign import ccall "readv" c_readv :: Fd -> Ptr Iovec -> CInt -> IO #type ssize_t

readv :: PrepareIovec a => Fd -> HeteroPtrList a -> IO #type ssize_t
readv fd ps = withIovec ps $ c_readv fd

foreign import ccall "writev" c_writev :: Fd -> Ptr Iovec -> CInt -> IO #type ssize_t

writev :: PrepareIovec a => Fd -> HeteroPtrList a -> IO #type ssize_t
writev fd ps = withIovec ps $ c_writev fd
