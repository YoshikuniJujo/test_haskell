{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Arrow
import Control.Monad
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
import Control.Monad.Yaftee.IO qualified as IO
import Data.MonoTraversable
import Data.Vector qualified as V
import Data.Word
import Data.ByteString.FingerTree qualified as BSF
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
	let	rs = (+ 1) <$> Header.headerToRows hdr

	h <- openFile fp ReadMode
	ibd <- PipeZ.cByteArrayMalloc 64
	obd <- PipeZ.cByteArrayMalloc 64

	void . Eff.runM . Except.run @String . Except.run @Zlib.ReturnCode
		. Buffer.run @"foobar" @BSF.ByteString
		. PipeZ.run @"foobar"
		. flip (State.runN @"foobar") palette0
		. Steps.chunkRun_ @"foobar"
		. Pipe.run
		. (`Except.catch` IO.print @Zlib.ReturnCode)
		. (`Except.catch` IO.putStrLn)
		. void $ PipeBS.hGet 32 h
			Pipe.=$= PipeT.convert BSF.fromStrict
			Pipe.=$= Steps.chunk "foobar"
			Pipe.=$= forever do
				bs <- Pipe.await
				cnk <- State.getN @Steps.Chunk "foobar"
				if (cnk == Steps.Chunk "PLTE")
				then do
					IO.print $ readPalette bs
					State.putN "foobar" $ readPalette bs
				else if (cnk == Steps.Chunk "IDAT")
				then Pipe.yield bs
				else do
					IO.print bs
					IO.print cnk
			Pipe.=$= PipeZ.inflate "foobar" IO (Zlib.WindowBitsZlib 15) ibd obd
			Pipe.=$= Buffer.format "foobar" BSF.splitAt' "" rs
			Pipe.=$= Unfilter.pngUnfilter' hdr
			Pipe.=$= do
				r <- Pipe.await
				plt <- State.getN "foobar"
				Pipe.yield $ (lookupPalette plt) <$> r
				forever $
					Pipe.yield . (lookupPalette plt <$>) =<< Pipe.await
			Pipe.=$= PipeIO.print

data Palette = Palette (V.Vector (Word8, Word8, Word8)) deriving Show

palette0 = Palette $ V.empty

readPalette :: BSF.ByteString -> Palette
readPalette = Palette . V.unfoldr \bs -> case BSF.splitAt' 3 bs of
	Nothing -> Nothing
	Just (otoList -> [r, g, b], bs') -> Just ((r, g, b), bs')

lookupPalette :: (RealFrac d, Integral i) => Palette -> i -> Rgb d
lookupPalette (Palette v) i = let (r, g, b) = v V.! fromIntegral i in RgbWord8 r g b
