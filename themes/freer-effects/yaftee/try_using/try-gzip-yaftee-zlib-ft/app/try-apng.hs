{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Arrow
import Control.Monad
import Control.Monad.ST
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.IO qualified as PipeIO
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeBS
import Control.Monad.Yaftee.Pipe.Buffer qualified as Buffer
import Control.Monad.Yaftee.Pipe.Png.Decode qualified as Png
import Control.Monad.Yaftee.Pipe.Png.Decode.Unfilter qualified as Unfilter
import Control.Monad.Yaftee.Pipe.Png.Decode.Steps qualified as Steps
import Control.Monad.Yaftee.Pipe.Zlib qualified as PipeZ
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.Fail qualified as Fail
import Control.Monad.Yaftee.IO qualified as IO
import Control.HigherOpenUnion qualified as U
import Data.Bits
import Data.Word
import Data.ByteString.FingerTree qualified as BSF
import Data.ByteString.FingerTree.Bits qualified as BSF
import Data.Color
import Data.Png.Header qualified as Header

import System.IO
import System.Environment

import Codec.Compression.Zlib.Constant.Core qualified as Zlib
import Codec.Compression.Zlib.Advanced.Core qualified as Zlib

main :: IO ()
main = do
	fp : _ <- getArgs

	hh <- openFile fp ReadMode
	Right hdr <- Eff.runM . Except.run @String
		. Png.runHeader @"foobar" . Pipe.run
		. void $ PipeBS.hGet 32 hh
			Pipe.=$= PipeT.convert BSF.fromStrict
			Pipe.=$= Png.decodeHeader "foobar"
	hClose hh

	print hdr

	h <- openFile fp ReadMode
	ibd <- PipeZ.cByteArrayMalloc 64
	obd <- PipeZ.cByteArrayMalloc 64

	void . Eff.runM . Except.run @String . Except.run @Zlib.ReturnCode
		. Buffer.run @"foobar" @BSF.ByteString
		. (`State.run` (0 :: Int))
		. (`State.run` fctl0)
		. Steps.chunkRun_ @"foobar"
		. Fail.runExc id id
		. PipeZ.run @"foobar"
		. Pipe.run
		. (`Fail.catch` IO.putStrLn)
		. (`Except.catch` IO.print @Zlib.ReturnCode)
		. (`Except.catch` IO.putStrLn)
		. void $ PipeBS.hGet 32 h
			Pipe.=$= PipeT.convert BSF.fromStrict
			Pipe.=$= apngPipe hdr ibd obd
			Pipe.=$= PipeIO.print'


apngPipe :: (
	U.Member Pipe.P es,
	Steps.ChunkMembers "foobar" es,
	U.Member (State.Named "foobar" (Maybe PipeZ.ByteString)) es,
	U.Member (State.Named "foobar" (Buffer.Monoid BSF.ByteString)) es,
	U.Member (State.S Int) es,
	U.Member (State.S Fctl) es,
	U.Member (Except.E String) es,
	U.Member (Except.E Zlib.ReturnCode) es,
	U.Member U.Fail es, U.Base IO.I es ) =>
	Header.Header ->
	PipeZ.CByteArray RealWorld -> PipeZ.CByteArray RealWorld ->
	Eff.E es BSF.ByteString [Rgba Double] ()
apngPipe hdr ibd obd = void $ Steps.chunk "foobar"
	Pipe.=$= forever do
		bs <- Pipe.await
		cn@Steps.Chunk {
			Steps.chunkBegin = cb,
			Steps.chunkName = cnn
			} <- State.getN "foobar"
		when (cb && cnn == "IDAT") $ Pipe.yield ""
		when (cnn == "IDAT") $ Pipe.yield bs
--		IO.print bs
		when cb do
			if bs == "" && cnn == "fdAT"
			then do
				bs <- Pipe.await
				IO.print "hogepiyofuga"
				let Just (srn :: Word32, bs') = (BSF.toBitsBE `first`) <$> BSF.splitAt' 4 bs
				IO.print srn
				Pipe.yield ""
				Pipe.yield bs'
			else IO.print "foobarbaz"
		when (not cb && cnn == "fdAT") $ Pipe.yield bs
		IO.print cn
		when (bs /= "") $ printOneChunk cn bs
	Pipe.=$= do
		"" <- Pipe.await
		n <- State.get
		Pipe.yield ""
		replicateM n do
			"" <- Pipe.await
			Pipe.yield "\123"
			fctl <- State.get
			IO.print $ (+ 1) <$> Header.headerToRows' hdr
				(fctlWidth fctl) (fctlHeight fctl)
			PipeZ.inflate "foobar" IO (Zlib.WindowBitsZlib 15) ibd obd
	Pipe.=$= do
		"" <- Pipe.await
		n <- State.get
		Pipe.yield ""
		replicateM n do
			until123
--			"\123" <- Pipe.await
			fctl <- State.get
			let	rs  = (+ 1) <$> Header.headerToRows' hdr
					(fctlWidth fctl) (fctlHeight fctl)
			IO.print rs
			Pipe.yield ""
			Buffer.format "foobar" BSF.splitAt' "" rs
	Pipe.=$= do
		"" <- Pipe.await
		n <- State.get
		replicateM_ n do
			"" <- Pipe.await
			fctl <- State.get
			Unfilter.pngUnfilter'' hdr (fromIntegral $ fctlHeight fctl)
	Pipe.=$= PipeT.convert (Header.word8ListToRgbaList @Double hdr)

until123 :: U.Member Pipe.P effs => Eff.E effs BSF.ByteString o ()
until123 = do
	bs <- Pipe.await
	if bs == "\123"
	then pure ()
	else do
		when (bs /= "") $ error "bad"
		until123

printOneChunk :: (
	U.Member (State.S Int) effs, U.Member (State.S Fctl) effs ) =>
	U.Base IO.I effs => Steps.Chunk -> BSF.ByteString -> Eff.E effs i o ()
printOneChunk (Steps.Chunk { Steps.chunkName = "acTL" }) bs = do
	let	x@(Just (fn, _)) = (BSF.toBitsBE *** BSF.toBitsBE) <$> BSF.splitAt' 4 bs
	State.put fn
	IO.print @(Maybe (Int, Word32)) x
printOneChunk (Steps.Chunk { Steps.chunkName = "fcTL" }) "" = pure ()
printOneChunk (Steps.Chunk { Steps.chunkName = "fcTL" }) bs = do
	let	fctl = decodeFctl bs
	State.put fctl
	IO.print fctl
printOneChunk _ _ = pure ()

data Fctl = Fctl {
	fctlSequenceNumber :: Word32,
	fctlWidth :: Word32, fctlHeight :: Word32,
	fctlXOffset :: Word32, fctlYOffset :: Word32,
	fctlDelayNum :: Word16, fctlDelayDen :: Word16,
	fctlDisposeOp :: Word8, fctlBlendOp :: Word8 } deriving Show

fctl0 :: Fctl
fctl0 = Fctl 0 0 0 0 0 0 0 0 0

decodeFctl :: BSF.ByteString -> Fctl
decodeFctl bs = let
	Just (sn, bs2) = splitBits bs
	Just (w, bs3) = splitBits bs2
	Just (h, bs4) = splitBits bs3
	Just (x, bs5) = splitBits bs4
	Just (y, bs6) = splitBits bs5
	Just (dn, bs7) = splitBits bs6
	Just (dd, bs8) = splitBits bs7
	Just (dpo, bs9) = splitBits bs8
	Just (blo, _bs10) = splitBits bs9 in Fctl {
	fctlSequenceNumber = sn,
	fctlWidth = w, fctlHeight = h,
	fctlXOffset = x, fctlYOffset = y,
	fctlDelayNum = dn, fctlDelayDen = dd,
	fctlDisposeOp = dpo, fctlBlendOp = blo }

splitBits ::
	forall b . FiniteBits b => BSF.ByteString -> Maybe (b, BSF.ByteString)
splitBits bs =
	(BSF.toBitsBE `first`) <$> BSF.splitAt' (finiteBitSize @b undefined `div` 8) bs
