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
import Control.Monad.Yaftee.Pipe.Png.Decode.Steps qualified as Steps
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.IO qualified as IO
import Data.MonoTraversable
import Data.Vector qualified as V
import Data.Word
import Data.ByteString.FingerTree qualified as BSF
import System.IO
import System.Environment

main :: IO ()
main = do
	fp : _ <- getArgs
	h <- openFile fp ReadMode
	void . Eff.runM . Except.run @String
		. Steps.chunkRun_ @"foobar"
		. Pipe.run
		. void $ PipeBS.hGet 32 h
			Pipe.=$= PipeT.convert BSF.fromStrict
			Pipe.=$= Steps.chunk "foobar"
			Pipe.=$= forever do
				bs <- Pipe.await
				cnk <- State.getN @Steps.Chunk "foobar"
				IO.print bs
				IO.print cnk
				when (cnk == Steps.Chunk "PLTE") .
					IO.print $ readPalette bs

data Palette = Palette (V.Vector (Word8, Word8, Word8)) deriving Show

readPalette :: BSF.ByteString -> Palette
readPalette = Palette . V.unfoldr \bs -> case BSF.splitAt' 3 bs of
	Nothing -> Nothing
	Just (otoList -> [r, g, b], bs') -> Just ((r, g, b), bs')
