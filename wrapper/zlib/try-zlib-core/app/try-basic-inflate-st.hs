{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.C.String
import Control.Monad.Primitive
import Control.Monad.ST
import Data.ByteString qualified as BS
import System.Environment

import Codec.Compression.Zlib.Structure.Core
import Codec.Compression.Zlib.Constant.Core
import Codec.Compression.Zlib.Basic.Core

main :: IO ()
main = do
	fp : _ <- getArgs
	cnt <- BS.readFile fp
	BS.putStr $ inflate cnt

inflate :: BS.ByteString -> BS.ByteString
inflate cnt =
	runST $ useAsCStringLen cnt \(p, ln) -> allocaBytesSt (64 * 64) \o -> do
		strm <- streamThaw streamInitial {
			streamNextIn = castPtr p,
			streamAvailIn = fromIntegral ln,
			streamNextOut = o,
			streamAvailOut = 64 * 64 }
		withStreamPtr strm $ unsafeIOToPrim . c_inflateInit
		withStreamPtr strm $ unsafeIOToPrim . \s -> c_inflate s Finish
		withStreamPtr strm $ unsafeIOToPrim . c_inflateEnd
		ao <- availOut strm
		packCStringLen (castPtr o, 64 * 64 - fromIntegral ao)

useAsCStringLen :: PrimBase m =>
	BS.ByteString -> (CStringLen -> m a) -> m a
useAsCStringLen bs a =
	unsafeIOToPrim . BS.useAsCStringLen bs $ unsafePrimToIO . a

packCStringLen :: PrimMonad m => CStringLen -> m BS.ByteString
packCStringLen = unsafeIOToPrim . BS.packCStringLen

allocaBytesSt :: PrimBase m => Int -> (Ptr a -> m b) -> m b
allocaBytesSt n a = unsafeIOToPrim . allocaBytes n $ unsafePrimToIO . a
