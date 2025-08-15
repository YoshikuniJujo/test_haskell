{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Arrow
import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.IO qualified as PipeIO
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeBS
import Control.Monad.Yaftee.Pipe.Png.Decode qualified as Png
import Control.Monad.Yaftee.Pipe.Png.Decode.Steps qualified as Steps
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.Fail qualified as Fail
import Control.Monad.Yaftee.IO qualified as IO
import Control.HigherOpenUnion qualified as U
import Data.Bits
import Data.Word
import Data.ByteString.FingerTree qualified as BSF
import Data.ByteString.FingerTree.Bits qualified as BSF

import System.IO
import System.Environment

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

	void . Eff.runM . Except.run @String
		. Steps.chunkRun_ @"foobar"
		. Fail.run
		. Pipe.run
		. (`Fail.catch` IO.putStrLn)
		. (`Except.catch` IO.putStrLn)
		. void $ PipeBS.hGet 32 h
			Pipe.=$= PipeT.convert BSF.fromStrict
			Pipe.=$= Steps.chunk "foobar"
			Pipe.=$= forever do
				bs <- Pipe.await
				IO.print bs
				cn <- State.getN "foobar"
				IO.print @Steps.Chunk cn
				printOneChunk cn bs

printOneChunk ::
	U.Base IO.I effs => Steps.Chunk -> BSF.ByteString -> Eff.E effs i o ()
printOneChunk (Steps.Chunk "acTL") bs =
	IO.print @(Maybe (Word32, Word32)) $ (BSF.toBitsBE *** BSF.toBitsBE) <$> BSF.splitAt' 4 bs
printOneChunk (Steps.Chunk "fcTL") "" = pure ()
printOneChunk (Steps.Chunk "fcTL") bs = IO.print $ decodeFctl bs
printOneChunk _ _ = pure ()

data Fctl = Fctl {
	fctlSequenceNumber :: Word32,
	fctlWidth :: Word32, fctlHeight :: Word32,
	fctlXOffset :: Word32, fctlYOffset :: Word32,
	fctlDelayNum :: Word16, fctlDelayDen :: Word16,
	fctlDisposeOp :: Word8, fctlBlendOp :: Word8 } deriving Show

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
