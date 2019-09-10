{-# LANGUAGE CApiFFI #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module VectoredIo (readVector, writeVector, readv, writev) where

import Foreign.Ptr (Ptr)
import Foreign.C.Types (CInt(..))
import Control.Monad (when)
import Data.Int (Int64)
import System.IO
import System.Posix (Fd(..), handleToFd)

import Iovec (withIovec, Iovec, PluralPtrLen(..))

#include <sys/uio.h>

readVector :: PluralPtrLen pl => Handle -> [Int] -> IO (ValueLists pl)
readVector h ns = do
	fd <- handleToFd h
	allocaPluralPtrLen ns $ \pl ->
		readv fd pl >> peekPluralPtrLen pl

readv :: PluralPtrLen pl => Fd -> pl -> IO #type ssize_t
readv fd pns = do
	n <- withIovec pns $ c_readv fd
	(n <$) . when (n < 0) $ errno "c_readv" n

foreign import ccall "readv"
	c_readv :: Fd -> Ptr Iovec -> CInt -> IO #type ssize_t

writeVector :: PluralPtrLen pl => Handle -> ValueLists pl -> IO ()
writeVector h vs = do
	fd <- handleToFd h
	allocaPluralPtrLen (valueListLengths vs) $ \pl ->
		() <$ (pokePluralPtrLen pl vs >> writev fd pl)

writev :: PluralPtrLen pl => Fd -> pl -> IO #type ssize_t
writev fd pns = do
	n <- withIovec pns $ c_writev fd
	(n <$) . when (n < 0) $ errno "c_write" n

foreign import ccall "writev"
	c_writev :: Fd -> Ptr Iovec -> CInt -> IO #type ssize_t

errno :: String -> #{type ssize_t} -> IO ()
errno nm r = do
	en <- c_errno
	error $	nm ++ " return error: " ++ show r ++ "\nerrno: " ++ show en

foreign import capi "value errno" c_errno :: IO CInt
