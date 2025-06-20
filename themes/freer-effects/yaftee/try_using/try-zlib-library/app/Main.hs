{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Foreign.Ptr
import Foreign.C.ByteArray qualified as CByteArray
import Control.Monad
import Control.Monad.ToolsYj
import Control.Monad.Primitive
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeBS
import Control.Monad.Yaftee.State qualified as State
import Control.HigherOpenUnion qualified as U
import System.IO
import System.Environment

import Codec.Compression.Zlib.Structure.Core
import Codec.Compression.Zlib.Constant.Core
import Codec.Compression.Zlib.Basic.Core
import Codec.Compression.Zlib.Advanced.Core

import Data.ByteString.FingerTree as BSF
import Data.ByteString.FingerTree.CString as BSF

inputBufSize, outputBufSize :: Int
inputBufSize = 64 * 4
outputBufSize = 64

readBufSize :: Int
readBufSize = 64 * 5

main :: IO ()
main = do
	fpi : fpo : _ <- getArgs
	(hi, ho) <- (,) <$> openFile fpi ReadMode <*> openFile fpo WriteMode
	i <- CByteArray.malloc inputBufSize
	o <- CByteArray.malloc outputBufSize

	void . Eff.runM . (`State.run` (Nothing :: Maybe BSF.ByteString))
		. Pipe.run $ PipeBS.hGet readBufSize hi Pipe.=$=
			PipeT.convert BSF.fromStrict Pipe.=$=
			inflatePipe IO i o Pipe.=$=
			PipeT.convert BSF.toStrict Pipe.=$=
			PipeBS.hPutStr ho

	CByteArray.free i
	CByteArray.free o
	hClose ho; hClose hi

inflatePipe :: forall m -> (
	PrimBase m,
	U.Member Pipe.P es,
	U.Member (State.S (Maybe BSF.ByteString)) es,
	U.Base (U.FromFirst m) es ) =>
	CByteArray.B -> CByteArray.B ->
	Eff.E es BSF.ByteString BSF.ByteString ()
inflatePipe m (i, ni) (o, no) = do
	bs <- Pipe.await
	((i', n), ebs) <- Eff.effBase . unsafeIOToPrim @m $ BSF.poke (castPtr i, ni) bs
	either  (const $ pure ()) (State.put . Just ) ebs
	strm <- Eff.effBase $ streamThaw @m streamInitial {
		streamNextIn = castPtr i',
		streamAvailIn = fromIntegral n,
		streamNextOut = castPtr o,
		streamAvailOut = fromIntegral no }
	_ <- Eff.effBase (inflateInit2 @m strm (WindowBitsZlibAndGzip 15))

	doWhile_ do
		rc <- Eff.effBase (inflate @m strm NoFlush)
		ai <- Eff.effBase @m $ availIn strm
		ao <- Eff.effBase @m $ availOut strm
		Pipe.yield =<< Eff.effBase (unsafeIOToPrim @m
			$ BSF.peek (castPtr o, no - fromIntegral ao))
		when (rc /= StreamEnd && ai == 0) do
			bs' <- getInput
			((i'', n'), ebs') <- Eff.effBase . unsafeIOToPrim @m $ BSF.poke (castPtr i, ni) bs'
			either (const $ pure ()) (State.put . Just) ebs'
			Eff.effBase $ nextIn @m strm (castPtr i'') (fromIntegral n')
		Eff.effBase . nextOut @m strm o $ fromIntegral no
		pure $ rc /= StreamEnd

	_ <- Eff.effBase (inflateEnd @m strm)
	pure ()

getInput :: forall es i o . (
	U.Member Pipe.P es,
	U.Member (State.S (Maybe i)) es ) =>
	Eff.E es i o i
getInput = State.get >>= \case
	Nothing -> Pipe.await
	Just bs -> bs <$ State.put @(Maybe i) Nothing
