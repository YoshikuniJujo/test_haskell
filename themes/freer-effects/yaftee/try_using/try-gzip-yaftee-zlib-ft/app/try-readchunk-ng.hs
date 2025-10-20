{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Control.Monad.ToolsYj
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
import Control.HigherOpenUnion qualified as U
import Data.Foldable
import Data.Word.Word8 qualified as Word8
import Data.Word.Crc32 qualified as Crc32
import Data.ByteString.FingerTree qualified as BSF
import Data.Png qualified as Png
import System.IO
import System.Environment

main :: IO ()
main = do
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

			doWhile_ $ chunk1 "foobar" 10

		Pipe.=$= PipeIO.print

chunk1 :: forall nm -> (
	U.Member Pipe.P es, Bytes.BytesMembers nm es,
	U.Member (Except.E String) es ) =>
	Int -> Eff.E es BSF.ByteString Chunk Bool
chunk1 nm m = do
	n <- Word8.toBitsBE <$> Bytes.readBytes nm 4
	Bytes.resetCrc32 nm
	cnm <- Bytes.readBytes nm 4
	Pipe.yield $ ChunkBegin cnm
	for_ (split m n) \n' -> Pipe.yield . ChunkBody =<< Bytes.readBytes nm n'
	Bytes.compCrc32 nm
	Bytes.Crc32 crc1 <- State.getN nm
	crc0 <- Crc32.fromWord . Word8.toBitsBE <$> Bytes.readBytes nm 4
	when (crc1 /= crc0) $ Except.throw @String "chunk1: CRC32 error"
	Pipe.yield ChunkEnd
	pure $ cnm /= "IEND"
	where
	split n = fix \go -> \case
		0 -> []
		m'	| n < m' -> n : go (m' - n)
			| otherwise -> [m']

data Chunk
	= ChunkBegin BSF.ByteString
	| ChunkBody BSF.ByteString | ChunkEnd deriving Show
