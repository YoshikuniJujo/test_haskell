{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.Png.Decode
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeBS
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.Fail qualified as Fail
import Control.Monad.Yaftee.IO qualified as IO
import Data.TypeLevel.List
import Data.Word
import Data.ByteString qualified as BS
import Data.CairoImage.Internal
import Data.Png.Header
import System.IO
import System.Environment
import Graphics.Pipe.Write
import Graphics.Pipe.Draw

import Pipe.Huffman qualified as Huffman

main :: IO ()
main = do
	fpi : fpo : _ <- getArgs
	hh <- openFile fpi ReadMode

	(_, hdr) <- Eff.runM . (`State.run` header0)
		. Except.run @String . Fail.runExc id . pngRunNew @"chunk" @"deflate" . Pipe.run
		$ PipeBS.hGet (64 * 64) hh Pipe.=$= pngHeader "chunk" "deflate" \hdr -> do
			IO.print hdr
			State.put hdr
	hClose hh

	let	(fromIntegral -> wdt, fromIntegral -> hgt) = (headerWidth hdr, headerHeight hdr)
		ct = headerColorType hdr

	print ct

	case ct of
		ColorTypeColorAlpha -> do

			h <- openFile fpi ReadMode

			img <- newImageMut @Argb32Mut wdt hgt
			writeDrawPipe Effs fpo (fromIntegral wdt) (fromIntegral hgt) img
				(Except.run
					. Fail.runExc id
					. (`State.runN` Huffman.IsLiteral (const False))
					. (`State.runN` Huffman.PhaseOthers)
					. pngRunNew) $
				PipeBS.hGet (64 * 64) h Pipe.=$=
				(void (png' "chunk" "deflate" IO.print) `Except.catch` IO.print @String) Pipe.=$=
		--		PipeT.convert BS.tail Pipe.=$=
				drawCairoImageRgba32 IO img wdt hgt (pure ())

		ColorTypeColor -> do

			h <- openFile fpi ReadMode

			img <- newImageMut @Argb32Mut wdt hgt
			writeDrawPipe Effs fpo (fromIntegral wdt) (fromIntegral hgt) img
				(Except.run
					. Fail.runExc id
					. (`State.runN` Huffman.IsLiteral (const False))
					. (`State.runN` Huffman.PhaseOthers)
					. pngRunNew) $
				PipeBS.hGet (64 * 64) h Pipe.=$=
				(void (png' "chunk" "deflate" IO.print) `Except.catch` IO.print @String) Pipe.=$=
		--		PipeT.convert BS.tail Pipe.=$=
				drawCairoImageRgb24 IO img wdt hgt (pure ())

type Effs = PngStates' "chunk" "deflate" `Append` '[
	State.Named "" Huffman.Phase,
	State.Named "" (Huffman.IsLiteral Int),
	Fail.F, (Except.E String)
	]
