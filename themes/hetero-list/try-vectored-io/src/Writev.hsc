{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Writev where

import Data.Int
import Foreign.Ptr
import Foreign.C.Types
import System.Posix.Types

import Foreign.Storable
import Foreign.Marshal

import HeteroList
import Iovec

#include <sys/uio.h>

foreign import ccall "writev" c_writev :: Fd -> Ptr Iovec -> CInt -> IO #type ssize_t

writev :: PrepareIovec a => Fd -> HeteroPtrList a -> IO #type ssize_t
writev fd ps = withIovec ps $ c_writev fd

wsample :: Fd -> Int -> CChar -> IO ()
wsample fd i c = alloca $ \pint -> alloca $ \pc -> do
	poke pint i
	poke pc c
	writev fd (pint :-- pc :-- PtrNil) >>= print
