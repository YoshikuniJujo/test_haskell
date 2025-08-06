{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

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
-- import Control.Monad.Yaftee.Pipe.Png.Encode qualified as Encode
import Control.Monad.Yaftee.Pipe.Png.Palette qualified as Encode
import Control.Monad.Yaftee.Pipe.Zlib qualified as PipeZ
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.IO qualified as IO
import Data.Maybe
import Data.Word
import Data.ByteString.FingerTree qualified as BSF
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
		. flip (State.runN @"foobar") Encode.palette0
		. flip (State.runN @"foobar") (Encode.palette0', [] :: [Word8])
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
					pllt <- State.getN "foobar"
--					IO.print $ Encode.readPalette bs
					IO.print $ Encode.readPalette2 pllt bs
					State.putN "foobar" $ Encode.readPalette2 pllt bs
					State.putN "foobar" $ Encode.readPalette bs
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
				Pipe.yield $ (Encode.lookupPalette @Double plt) <$> r
				forever $
					Pipe.yield . (Encode.lookupPalette plt <$>) =<< Pipe.await
			Pipe.=$= do
				p <- Pipe.await
				plt <- State.getN "foobar"
				Pipe.yield $ fromJust . Encode.elemIndexPalette @_ @Int plt <$> p
				forever $
					Pipe.yield . ((fromJust . Encode.elemIndexPalette plt) <$>) =<< Pipe.await
			Pipe.=$= PipeIO.print
