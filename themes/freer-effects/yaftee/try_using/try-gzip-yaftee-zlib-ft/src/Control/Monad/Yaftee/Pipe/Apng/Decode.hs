{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.Apng.Decode (
	apngRun_, ApngStates, apngPipe, ApngMembers,
	FrameNumber(..), Body(..), Fctl(..),
	fctlPoss, fctlPoss' ) where

import Control.Arrow
import Control.Monad
import Control.Monad.ST
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.Buffer qualified as Buffer
import Control.Monad.Yaftee.Pipe.Png.Decode.Unfilter qualified as Unfilter
import Control.Monad.Yaftee.Pipe.Png.Decode.Steps qualified as Steps
import Control.Monad.Yaftee.Pipe.Zlib qualified as PipeZ
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.IO qualified as IO
import Control.HigherOpenUnion qualified as U
import Data.TypeLevel.List
import Data.HigherFunctor qualified as HFunctor
import Data.Bits
import Data.Word
import Data.ByteString.FingerTree qualified as BSF
import Data.ByteString.FingerTree.Bits qualified as BSF
import Data.Color
import Data.Png.Header qualified as Header

import Codec.Compression.Zlib.Constant.Core qualified as Zlib
import Codec.Compression.Zlib.Advanced.Core qualified as Zlib

import Control.Monad.Yaftee.Pipe.Png.Encode qualified as PngE

type ApngStates nm =
	State.Named nm (Maybe PipeZ.ByteString) ':
	Steps.ChunkStates nm `Append` '[
	State.Named nm Fctl,
	State.Named nm FrameNumber,
	State.Named nm (Buffer.Monoid BSF.ByteString) ]

apngRun_ :: forall nm es i o r .
	HFunctor.Loose (U.U es) =>
	Eff.E (ApngStates nm `Append` es) i o r -> Eff.E es i o ()
apngRun_ = void . Buffer.run @nm @BSF.ByteString
		. (\m -> State.runN @nm m (FrameNumber 0))
		. (\m -> State.runN @nm m fctl0)
		. Steps.chunkRun_ @nm
		. PipeZ.run @nm

newtype FrameNumber = FrameNumber Int deriving Show

type ApngMembers nm es = (
	Steps.ChunkMembers nm es,
	U.Member (State.Named nm (Maybe PipeZ.ByteString)) es,
	U.Member (State.Named nm (Buffer.Monoid BSF.ByteString)) es,
	U.Member (State.Named nm FrameNumber) es,
	U.Member (State.Named nm Fctl) es )

apngPipe :: forall es . forall nm -> (
	U.Member Pipe.P es,

	ApngMembers nm es,

	U.Member (Except.E Zlib.ReturnCode) es,
	U.Member (Except.E String) es,

	U.Member U.Fail es, U.Base IO.I es ) =>
	Header.Header ->
	PipeZ.CByteArray RealWorld -> PipeZ.CByteArray RealWorld ->
	Eff.E es BSF.ByteString Body ()
apngPipe nm hdr ibd obd = void $ Steps.chunk nm
	Pipe.=$= forever do
		bs <- Pipe.await
		cn@Steps.Chunk {
			Steps.chunkBegin = cb,
			Steps.chunkName = cnn
			} <- State.getN nm
		when (cb && cnn == "IDAT") $ Pipe.yield ""
		when (cnn == "IDAT") $ Pipe.yield bs
--		IO.print bs
		when cb do
			if bs == "" && cnn == "fdAT"
			then do
				bs' <- Pipe.await
				IO.print @String "hogepiyofuga"
				Just (srn :: Word32, bs'') <- pure $ (BSF.toBitsBE `first`) <$> BSF.splitAt' 4 bs'
				IO.print srn
				Pipe.yield ""
				Pipe.yield bs''
			else IO.print @String "foobarbaz"
		when (not cb && cnn == "fdAT") $ Pipe.yield bs
		IO.print cn
		when (bs /= "") $ printOneChunk nm cn bs
	Pipe.=$= do
		"" <- Pipe.await
		FrameNumber n <- State.getN nm
		Pipe.yield ""
		replicateM n do
			"" <- Pipe.await
			Pipe.yield "\123"
			fctl <- State.getN nm
			IO.print $ (+ 1) <$> Header.headerToRows' hdr
				(fctlWidth fctl) (fctlHeight fctl)
			PipeZ.inflate nm IO (Zlib.WindowBitsZlib 15) ibd obd
	Pipe.=$= do
		"" <- Pipe.await
		FrameNumber n <- State.getN nm
		Pipe.yield ""
		replicateM n do
			until123
--			"\123" <- Pipe.await
			fctl <- State.getN nm
			let	rs  = (+ 1) <$> Header.headerToRows' hdr
					(fctlWidth fctl) (fctlHeight fctl)
			IO.print rs
			Pipe.yield ""
			Buffer.format nm BSF.splitAt' "" rs
	Pipe.=$= do
		"" <- Pipe.await
		FrameNumber n <- State.getN nm
		Pipe.yield BodyNull
		replicateM_ n do
			"" <- Pipe.await
			fctl <- State.getN nm
			Pipe.yield $ BodyFctl fctl
			Unfilter.pngUnfilter'' hdr (fromIntegral $ fctlHeight fctl)
				Pipe.=$= PipeT.convert (BodyRgba . Header.word8ListToRgbaList @Double hdr)
			Pipe.yield BodyFdatEnd
		Pipe.yield BodyEnd

until123 :: U.Member Pipe.P effs => Eff.E effs BSF.ByteString o ()
until123 = do
	bs <- Pipe.await
	if bs == "\123"
	then pure ()
	else do
		when (bs /= "") $ error "bad"
		until123

printOneChunk :: forall effs i o . forall nm -> (
	U.Member (State.Named nm FrameNumber) effs, U.Member (State.Named nm Fctl) effs,
	U.Member U.Fail effs ) =>
	U.Base IO.I effs => Steps.Chunk -> BSF.ByteString -> Eff.E effs i o ()
printOneChunk nm (Steps.Chunk { Steps.chunkName = "acTL" }) bs = do
	x@(Just (fn, _)) <- pure $ (BSF.toBitsBE *** BSF.toBitsBE) <$> BSF.splitAt' 4 bs
	State.putN nm $ FrameNumber fn
	IO.print @(Maybe (Int, Word32)) x
printOneChunk _ (Steps.Chunk { Steps.chunkName = "fcTL" }) "" = pure ()
printOneChunk nm (Steps.Chunk { Steps.chunkName = "fcTL" }) bs = do
	Just fctl <- pure $ decodeFctl bs
	State.putN nm fctl
	IO.print fctl
printOneChunk _ _ _ = pure ()

data Body
	= BodyNull | BodyEnd | BodyFdatEnd
	| BodyFctl Fctl | BodyRgba [Rgba Double] deriving Show

instance PngE.Datable Body where
	isDat (BodyRgba _) = True
	endDat = \case BodyFdatEnd -> True; _ -> False
	toDat hdr (BodyRgba rgba) = BSF.pack $ Header.rgbaListToWord8List hdr rgba

data Fctl = Fctl {
	fctlSequenceNumber :: Word32,
	fctlWidth :: Word32, fctlHeight :: Word32,
	fctlXOffset :: Word32, fctlYOffset :: Word32,
	fctlDelayNum :: Word16, fctlDelayDen :: Word16,
	fctlDisposeOp :: Word8, fctlBlendOp :: Word8 } deriving Show

fctlPoss :: Header.Header -> Fctl -> [(Int, Int)]
fctlPoss hdr fctl = Header.calcPoss hdr (fctlWidth fctl) (fctlHeight fctl)

fctlPoss' hdr fctl = Header.calcPoss' hdr (fctlWidth fctl) (fctlHeight fctl)

fctl0 :: Fctl
fctl0 = Fctl 0 0 0 0 0 0 0 0 0

decodeFctl :: BSF.ByteString -> Maybe Fctl
decodeFctl bs = do
	(sn, bs2) <- splitBits bs
	(w, bs3) <- splitBits bs2
	(h, bs4) <- splitBits bs3
	(x, bs5) <- splitBits bs4
	(y, bs6) <- splitBits bs5
	(dn, bs7) <- splitBits bs6
	(dd, bs8) <- splitBits bs7
	(dpo, bs9) <- splitBits bs8
	(blo, _bs10) <- splitBits bs9
	pure Fctl {
		fctlSequenceNumber = sn,
		fctlWidth = w, fctlHeight = h,
		fctlXOffset = x, fctlYOffset = y,
		fctlDelayNum = dn, fctlDelayDen = dd,
		fctlDisposeOp = dpo, fctlBlendOp = blo }

splitBits ::
	forall b . FiniteBits b => BSF.ByteString -> Maybe (b, BSF.ByteString)
splitBits bs =
	(BSF.toBitsBE `first`) <$> BSF.splitAt' (finiteBitSize @b undefined `div` 8) bs
