{-# LANGUAGE CApiFFI #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module VectoredIo (readVector, writeVector, readv, writev) where

import Foreign.Ptr (Ptr)
import Foreign.C.Types (CInt(..))
import Control.Monad (when)
import Data.Int (Int64)
import System.IO (Handle)
import System.Posix (Fd(..), handleToFd)

import qualified Data.ByteString as BS

import Iovec (
	Iovec, withIovec, PluralPtrLen(..),
	byteLengthPluralPtrLen, peekByteStringPluralPtrLen)

#include <sys/uio.h>

readVector :: PluralPtrLen ppl => Handle -> [Int] -> IO (Either [BS.ByteString] (ValueLists ppl))
readVector h ns = handleToFd h >>= \fd ->
	allocaPluralPtrLen ns $ \ppl -> do
		let n0 = byteLengthPluralPtrLen ppl
		n <- fromIntegral <$> readv fd ppl
		if n < n0
		then Left <$> peekByteStringPluralPtrLen ppl n
		else Right <$> peekPluralPtrLen ppl

readv :: PluralPtrLen ppl => Fd -> ppl -> IO #type ssize_t
readv fd pns = do
	n <- withIovec pns $ c_readv fd
	(n <$) . when (n < 0) $ errno "c_readv" n

foreign import ccall "readv"
	c_readv :: Fd -> Ptr Iovec -> CInt -> IO #type ssize_t

writeVector :: PluralPtrLen ppl => Handle -> ValueLists ppl -> IO ()
writeVector h vls = handleToFd h >>= \fd ->
	allocaPluralPtrLen (valueListLengths vls) $ \ppl -> do
		pokePluralPtrLen ppl vls
		let n0 = byteLengthPluralPtrLen ppl
		n <- fromIntegral <$> writev fd ppl
		when (n < n0) . error $ "can't write enough length:\n" ++
			"should write: " ++ show n0 ++
			"actual write: " ++ show n

writev :: PluralPtrLen ppl => Fd -> ppl -> IO #type ssize_t
writev fd pns = do
	n <- withIovec pns $ c_writev fd
	(n <$) . when (n < 0) $ errno "c_write" n

foreign import ccall "writev"
	c_writev :: Fd -> Ptr Iovec -> CInt -> IO #type ssize_t

errno :: String -> #{type ssize_t} -> IO ()
errno nm r = c_errno >>= \en ->
	error $	nm ++ " return error: " ++ show r ++ "\nerrno: " ++ show en

foreign import capi "value errno" c_errno :: IO CInt
