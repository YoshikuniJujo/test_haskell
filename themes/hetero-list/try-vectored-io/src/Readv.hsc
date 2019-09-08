{-# LANGUAGE DataKinds, TypeOperators, GADTs, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Readv where

import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal
import Foreign.C.Types
import Data.Int
import System.Posix.Types

import HeteroList
import Iovec

#include <sys/uio.h>

foreign import ccall "readv" c_readv :: Fd -> Ptr Iovec -> CInt -> IO #type ssize_t

readv :: PrepareIovec a => Fd -> HeteroPtrList a -> IO #type ssize_t
readv fd ps = withIovec ps $ c_readv fd

sample :: Fd -> IO (Int, CChar)
sample fd = alloca $ \pint -> alloca $ \pc -> do
	readv fd (pint :-- pc :-- PtrNil) >>= print
	(,) <$> peek pint <*> peek pc
