{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.IO qualified as PipeIO
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeBS
import Control.Monad.Yaftee.Pipe.BytesCrc32 qualified as Bytes
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.IO qualified as IO
import Data.Foldable
import Data.Word
import Data.Word.Word8 qualified as Word8
import Data.Word.Crc32 qualified as Crc32
import Data.ByteString.FingerTree qualified as BSF
import Data.Png qualified as Png
import System.IO
import System.Environment

main :: IO ()
main = do
	let	m = 10
	fp : _ <- getArgs
	putStrLn fp
	h <- openFile fp ReadMode
	void . Eff.runM
		. Bytes.bytesRun_ @"foobar" . Except.run @String
		. Pipe.run . (`Except.catch` IO.putStrLn) . void $ PipeBS.hGet 32 h
		Pipe.=$= PipeT.convert BSF.fromStrict
		Pipe.=$= do
			fhdr <- Bytes.readBytes "foobar" 8
			when (fhdr /= Png.fileHeader)
				$ Except.throw @String "file header error"
			n <- Word8.toBitsBE <$> Bytes.readBytes "foobar" 4
			Bytes.resetCrc32 "foobar"
			cnm <- Bytes.readBytes "foobar" 4
			Pipe.yield $ ChunkName cnm
			for_ (split m n) \n' -> Pipe.yield . ChunkBody =<< Bytes.readBytes "foobar" n'
--			IO.print =<< Bytes.readBytes "foobar" n
			Bytes.compCrc32 "foobar"
			Bytes.Crc32 crc1 <- State.getN "foobar"
			IO.print crc1
			IO.print . Crc32.fromWord =<< Word8.toBitsBE <$> Bytes.readBytes "foobar" 4
		Pipe.=$= PipeIO.print
		where
		split n = fix \go -> \case
			0 -> []
			m'	| n < m' -> n : go (m' - n)
				| otherwise -> [m']

data Chunk = ChunkName BSF.ByteString | ChunkBody BSF.ByteString deriving Show
