{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
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
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.IO qualified as PipeIO
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeBS
import Control.Monad.Yaftee.State qualified as State
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
import Data.ByteString.FingerTree.CString as BSF

main :: IO ()
main = do
	fpi : fpo : _ <- getArgs
	hi <- openFile fpi ReadMode
	ho <- openFile fpo WriteMode
	void . Eff.runM . (`State.run` (Nothing :: Maybe BSF.ByteString)) . Pipe.run
		$ PipeBS.hGet (64 * 5) hi Pipe.=$=
			PipeT.convert BSF.fromStrict Pipe.=$=
			inflatePipe IO Pipe.=$=
			PipeT.convert BSF.toStrict Pipe.=$=
			PipeBS.hPutStr ho
	hClose ho
	hClose hi

inflatePipe :: forall m -> (
	PrimBase m,
	U.Member Pipe.P es,
	U.Member (State.S (Maybe BSF.ByteString)) es,
	U.Base (U.FromFirst m) es ) =>
	Eff.E es BSF.ByteString BSF.ByteString ()
inflatePipe m = do
	i <- Eff.effBase . unsafeIOToPrim @m $ mallocBytes (64 * 4)
	o <- Eff.effBase . unsafeIOToPrim @m $  mallocBytes 64
	bs <- Pipe.await
	((i', n), ebs) <- Eff.effBase . unsafeIOToPrim @m $ BSF.poke (castPtr i, 64 * 4) bs
	either  (const $ pure ()) (State.put . Just ) ebs
	strm <- Eff.effBase $ streamThaw @m streamInitial {
		streamNextIn = castPtr i',
		streamAvailIn = fromIntegral n,
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
			$ BSF.peek (castPtr o, 64 - fromIntegral ao))
		when (rc /= StreamEnd && ai == 0) do
			bs <- getInput
			((i', n), ebs) <- Eff.effBase . unsafeIOToPrim @m $ BSF.poke (castPtr i, 64 * 4) bs
			either (const $ pure ()) (State.put . Just) ebs
			Eff.effBase $ nextIn @m strm (castPtr i') (fromIntegral n) -- i (fromIntegral $ BSF.length bs)
		Eff.effBase $ nextOut @m strm o 64
		pure $ rc /= StreamEnd

	Eff.effBase (inflateEnd @m strm)
	Eff.effBase . unsafeIOToPrim @m $ free i
	Eff.effBase . unsafeIOToPrim @m $ free o

getInput :: forall es i o . (
	U.Member Pipe.P es,
	U.Member (State.S (Maybe i)) es ) =>
	Eff.E es i o i
getInput = State.get >>= \case
	Nothing -> Pipe.await
	Just bs -> bs <$ State.put @(Maybe i) Nothing
