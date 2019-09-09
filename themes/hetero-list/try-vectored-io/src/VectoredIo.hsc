{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE GADTs, DataKinds, TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module VectoredIo (readv, writev) where

import Foreign.Ptr (Ptr)
import Foreign.C.Types (CInt(..))
import Control.Monad (when)
import Data.Int (Int64)
import System.Posix.Types (Fd(..))

import HeteroList (HeteroList, TypeMap)
import Iovec (Iovec, PrepareIovec, withIovec)

#include <sys/uio.h>

foreign import ccall "readv"
	c_readv :: Fd -> Ptr Iovec -> CInt -> IO #type ssize_t

readv :: PrepareIovec a => Fd -> HeteroList (Ptr `TypeMap` a) -> IO #type ssize_t
readv fd ps = do
	n <- withIovec ps $ c_readv fd
	(n <$) . when (n < 0) $ errorWithErrno "c_readv" n

foreign import ccall "writev"
	c_writev :: Fd -> Ptr Iovec -> CInt -> IO #type ssize_t

writev :: PrepareIovec a => Fd -> HeteroList (Ptr `TypeMap` a) -> IO #type ssize_t
writev fd ps = do
	n <- withIovec ps $ c_writev fd
	(n <$) . when (n < 0) $ errorWithErrno "c_writev" n

foreign import capi "value errno" c_errno :: IO CInt

errorWithErrno :: String -> #{type ssize_t} -> IO ()
errorWithErrno nm r = do
	en <- c_errno
	error $	nm ++ " return error: " ++ show r ++ "\n" ++
		"errno: " ++ show en
