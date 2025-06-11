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
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.Fail qualified as Fail
import Control.Monad.Yaftee.IO qualified as IO
import Data.Sequence qualified as Seq
import Data.Word
import Data.ByteString qualified as BS
import System.IO
import System.Environment

import Graphics.Pipe.Draw
import Data.Png.Header
import Data.Color

main :: IO ()
main = do
	fp : _ <- getArgs
	h <- openFile fp ReadMode
	Right hdr <- Eff.runM
		. Except.run @String . Png.runHeader @"foobar" . Pipe.run
		$ PipeBS.hGet 32 h Pipe.=$=
			PipeT.convert bsToSeq Pipe.=$=
			Png.decodeHeader "foobar"
	print hdr
	let	wdt = fromIntegral $ headerWidth hdr
		hgt = fromIntegral $ headerHeight hdr
	print $ headerInterlaceMethod hdr
	img <- newImageArgb32Mut wdt hgt

	h <- openFile fp ReadMode
	writeDrawPipe "foobar.png" img wdt hgt $ \img ->
		void . Eff.runM . Except.run @String
			. Fail.runExc id . Png.run_ @"foobar" . Pipe.run
			. (`Except.catch` IO.print @String)
			. void $ PipeBS.hGet 32 h Pipe.=$=
				PipeT.convert bsToSeq Pipe.=$=
				Png.decode @Double "foobar"
					(\_ -> pure ()) (\_ -> pure ()) Pipe.=$=
				PipeT.convert (either
					((`toRgba` AlphaWord8 255) <$>)
					id) Pipe.=$=
				drawColor img (repeat [0 ..])

bsToSeq :: BS.ByteString -> Seq.Seq Word8
bsToSeq = Seq.fromList . BS.unpack
