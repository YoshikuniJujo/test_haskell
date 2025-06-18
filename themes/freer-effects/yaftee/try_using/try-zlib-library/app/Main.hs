{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Control.Monad
import Control.Monad.ToolsYj
import Control.Monad.Primitive
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.IO qualified as PipeIO
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeBS
import Control.Monad.Yaftee.IO qualified as IO
import Control.HigherOpenUnion qualified as U
import Data.ByteString qualified as BS
import System.IO
import System.Environment
import Debug.Trace

import Codec.Compression.Zlib.Structure.Core
import Codec.Compression.Zlib.Constant.Core
import Codec.Compression.Zlib.Basic.Core
import Codec.Compression.Zlib.Advanced.Core

import Data.ByteString.FingerTree as BSF

main :: IO ()
main = do
	fp : _ <- getArgs
	h <- openFile fp ReadMode
	void . Eff.runM . Pipe.run
		$ PipeBS.hGet (64 * 4) h Pipe.=$=
			inflatePipe IO Pipe.=$=
			PipeBS.putStr

inflatePipe :: forall m -> (
	PrimBase m,
	U.Member Pipe.P es,
	U.Base (U.FromFirst m) es ) =>
	Eff.E es BS.ByteString BS.ByteString ()
inflatePipe m = do
	i <- Eff.effBase . unsafeIOToPrim @m $ mallocBytes (64 * 4)
	o <- Eff.effBase . unsafeIOToPrim @m $  mallocBytes 64
	bs <- Pipe.await
	Eff.effBase . unsafeIOToPrim @m $ pokeArray i (BS.unpack bs)
	strm <- Eff.effBase $ streamThaw @m streamInitial {
		streamNextIn = castPtr i,
		streamAvailIn = 64 * 4,
		streamNextOut = castPtr o,
		streamAvailOut = 64 }
	Eff.effBase (inflateInit2 @m strm (WindowBitsZlibAndGzip 15))

	doWhile_ do
		rc <- Eff.effBase (inflate @m strm NoFlush)
--		trace (show rc) (pure ())
		ai <- Eff.effBase @m $ availIn strm
		ao <- Eff.effBase @m $ availOut strm
--		trace (show ai ++ " " ++ show ao) (pure ())
		Pipe.yield =<< Eff.effBase (unsafeIOToPrim @m
			$ BS.packCStringLen (castPtr o, 64 - fromIntegral ao))
		when (rc /= StreamEnd && ai == 0) do
			bs <- Pipe.await
			Eff.effBase . unsafeIOToPrim @m $ pokeArray i (BS.unpack bs)
			Eff.effBase $ nextIn @m strm i (fromIntegral $ BS.length bs)
		Eff.effBase $ nextOut @m strm o 64
		pure $ rc /= StreamEnd

	Eff.effBase (inflateEnd @m strm)
	Eff.effBase . unsafeIOToPrim @m $ free i
	Eff.effBase . unsafeIOToPrim @m $ free o
