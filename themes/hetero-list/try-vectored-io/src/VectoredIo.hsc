{-# LANGUAGE CApiFFI #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module VectoredIo (readVector, writeVector, readv, writev) where

import Foreign.Ptr (Ptr)
import Foreign.C.Types (CInt(..))
import GHC.Stack (HasCallStack, callStack, prettyCallStack)
import Control.Monad (when)
import Data.Int (Int64)
import System.IO (Handle)
import System.Posix (Fd(..), handleToFd)

import qualified Data.ByteString as BS

import Iovec (
	Iovec, withIovec, PluralArray(..),
	pluralArrayByteLength, peekByteStringPluralArray )

#include <sys/uio.h>

readVector :: (HasCallStack, PluralArray ppl) =>
	Handle -> [Int] -> IO (Either [BS.ByteString] (ValueLists ppl))
readVector h ns = handleToFd h >>= \fd -> allocaPluralArray ns $ \ppl -> do
	let	n0 = pluralArrayByteLength ppl
	n <- fromIntegral <$> readv fd ppl
	case n `compare` n0 of
		LT -> Left <$> peekByteStringPluralArray ppl n
		EQ -> Right <$> peekPluralArray ppl
		GT -> error $
			"readVector: read more bytes than expected\n" ++
			"expected: " ++ show n0 ++ "bytes\n" ++
			"actual  : " ++ show n ++ "bytes"

readv :: (HasCallStack, PluralArray ppl) => Fd -> ppl -> IO #type ssize_t
readv fd pns = do
	n <- withIovec pns $ c_readv fd
	(n <$) . when (n < 0) $ errno "c_readv" n

foreign import ccall "readv"
	c_readv :: Fd -> Ptr Iovec -> CInt -> IO #type ssize_t

writeVector :: (HasCallStack, PluralArray ppl) => Handle -> ValueLists ppl -> IO ()
writeVector h vls = handleToFd h >>= \fd ->
	allocaPluralArray (valueListLengthList vls) $ \ppl -> do
		pokePluralArray ppl vls
		let	n0 = pluralArrayByteLength ppl
		n <- fromIntegral <$> writev fd ppl
		when (n < n0) . error $
			"writeVector: can't write enough length:\n" ++
			"expected: " ++ show n0 ++ "\n" ++
			"actual  : " ++ show n

writev :: (HasCallStack, PluralArray ppl) => Fd -> ppl -> IO #type ssize_t
writev fd pns = do
	n <- withIovec pns $ c_writev fd
	(n <$) . when (n < 0) $ errno "c_write" n

foreign import ccall "writev"
	c_writev :: Fd -> Ptr Iovec -> CInt -> IO #type ssize_t

errno :: HasCallStack => String -> #{type ssize_t} -> IO ()
errno nm r = c_errno >>= \en -> errorWithoutStackTrace $
	nm ++ " return error: " ++ show r ++ "\nerrno: " ++ show en ++ "\n" ++
	prettyCallStack callStack

foreign import capi "value errno" c_errno :: IO CInt
