{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ExplicitForAll, RequiredTypeArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.PngNg.Decode.Chunk (chunks, Chunk(..)) where

import Control.Monad
import Control.Monad.ToolsYj
import Control.Monad.Fix
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.BytesCrc32 qualified as Bytes
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.HigherOpenUnion qualified as U
import Data.Foldable
import Data.Word.Word8 qualified as Word8
import Data.Word.Crc32 qualified as Crc32
import Data.ByteString.FingerTree qualified as BSF
import Data.Png qualified as Png

chunks :: forall nm -> (
	U.Member Pipe.P es, Bytes.BytesMembers nm es,
	U.Member (Except.E String) es ) => Eff.E es BSF.ByteString Chunk ()
chunks nm = do
	fhdr <- Bytes.readBytes nm 8
	when (fhdr /= Png.fileHeader) $ Except.throw @String "file header error"
	doWhile_ $ chunk1 nm 10

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
