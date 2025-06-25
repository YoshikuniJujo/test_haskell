{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.IO qualified as PipeIO
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeBS
import Control.Monad.Yaftee.Pipe.Png.Decode qualified as Png
import Control.Monad.Yaftee.Pipe.Zlib qualified as PipeZ
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.Fail qualified as Fail
import Control.Monad.Yaftee.IO qualified as IO
import Data.ByteString.FingerTree qualified as BSF
import System.IO
import System.Environment

import Graphics.Pipe.Draw
import Data.Png.Header
import Data.Color

import Codec.Compression.Zlib.Constant.Core qualified as Zlib

import Lib

main :: IO ()
main = do
	fp : _ <- getArgs
	h <- openFile fp ReadMode
	ib <- PipeZ.cByteArrayMalloc 64
	ob <- PipeZ.cByteArrayMalloc 64
	Right hdr <- Eff.runM
		. Except.run @String
		. Png.runHeader @"foobar" . Pipe.run
		$ PipeBS.hGet 32 h Pipe.=$=
			PipeT.convert BSF.fromStrict Pipe.=$=
			Png.decodeHeader "foobar"
	print hdr
	let	wdt = fromIntegral $ headerWidth hdr
		hgt = fromIntegral $ headerHeight hdr
		ilm = headerInterlaceMethod hdr
		(yss, xss) = case ilm of
			InterlaceMethodNon -> ([0 ..] `zip` repeat (1, 1), repeat [0 ..])
			InterlaceMethodAdam7 -> (mkyss False hgt ++ [(0, (1, 1))], mkxss wdt hgt ++ [[]])
	img <- newImageArgb32Mut wdt hgt
	print (wdt, hgt)
	print img

	h' <- openFile fp ReadMode
	writeDrawPipe "foobar.png" img wdt hgt $ \img ->
		void . Eff.runM . Except.run @String
			. Except.run @Zlib.ReturnCode
			. Fail.runExc id . Png.run_ @"foobar" . Pipe.run
			. (`Except.catch` IO.print @Zlib.ReturnCode)
			. (`Except.catch` IO.print @String) . void
--			$ PipeBS.hGet 32 h' Pipe.=$=
			$ PipeBS.hGet 68 h' Pipe.=$=
				PipeT.convert BSF.fromStrict Pipe.=$=
				Png.decode @Double "foobar" IO ib ob Pipe.=$=
				PipeT.convert (either
					((`toRgba` AlphaWord8 255) <$>) id) Pipe.=$=
				drawColor img (fst <$> yss) xss
