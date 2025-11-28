{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module PngToImageGray1 (
	runPngToImageGray1, PngToImageGray1States,
	pngToImageGray1,
	PngToImageGray1Members ) where

import Control.Monad
import Control.Monad.ST
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.Buffer qualified as Buffer
import Control.Monad.Yaftee.Pipe.Png.Decode.Unfilter qualified as Unfilter
import Control.Monad.Yaftee.Pipe.Png.Decode.Steps qualified as Steps
import Control.Monad.Yaftee.Pipe.Png.Decode.Chunk qualified as Chunk
import Control.Monad.Yaftee.Pipe.Zlib qualified as PipeZ
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.IO qualified as IO
import Control.HigherOpenUnion qualified as U
import Data.TypeLevel.List
import Data.HigherFunctor qualified as HFunctor
import Data.ByteString qualified as BS
import Data.ByteString.FingerTree qualified as BSF
import Data.Png.Header qualified as Header
import Data.Png.Header.Data qualified as Header
import Data.Image.Gray1 qualified as ImageG1
import Codec.Compression.Zlib.Constant.Core qualified as Zlib
import Codec.Compression.Zlib.Advanced.Core qualified as Zlib

runPngToImageGray1 ::
	forall nm m es i o r . (HFunctor.Loose (U.U es), Monoid m) =>
	Eff.E (	PngToImageGray1States nm m `Append` es) i o r ->
	Eff.E es i o (r, Buffer.Monoid m)
runPngToImageGray1 = Buffer.run . PipeZ.run

type PngToImageGray1States nm m = '[
	State.Named nm (Maybe PipeZ.ByteString),
	State.Named nm (Buffer.Monoid m) ]

pngToImageGray1 :: forall nm -> (
	U.Member Pipe.P es, Chunk.ChunkMembers nm es,
	PngToImageGray1Members nm es,
	U.Member (Except.E String) es, U.Member (Except.E Zlib.ReturnCode) es,
	U.Base IO.I es ) =>
	Header.Header ->
	PipeZ.CByteArray RealWorld -> PipeZ.CByteArray RealWorld ->
	Eff.E es BS.ByteString ImageG1.G ()
pngToImageGray1 nm hdr ibd obd = void $ PipeT.convert BSF.fromStrict
	Pipe.=$= Steps.chunk nm
	Pipe.=$= forever do
		bs <- Pipe.await
		cnk <- State.getN @Chunk.Chunk nm
		when ("IDAT" `Chunk.isChunkName` cnk) $ Pipe.yield bs
	Pipe.=$= PipeZ.inflate nm IO (Zlib.WindowBitsZlib 15) ibd obd
	Pipe.=$= Buffer.format nm BSF.splitAt' "" rs
	Pipe.=$= Unfilter.pngUnfilter' hdr
	Pipe.=$= (Pipe.yield =<< ImageG1.generateFromBytesM
		(fromIntegral $ Header.headerWidth hdr)
		(fromIntegral $ Header.headerHeight hdr)
		Pipe.await)
	where rs = (+ 1) <$> Header.headerToRows hdr

type PngToImageGray1Members nm es = (
	U.Member (State.Named nm (Buffer.Monoid BSF.ByteString)) es,
	U.Member (State.Named nm (Maybe PipeZ.ByteString)) es )
