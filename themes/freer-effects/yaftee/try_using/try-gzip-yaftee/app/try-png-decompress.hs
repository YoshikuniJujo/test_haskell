{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.ToolsYj
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.IO qualified as PipeIO
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeBS
import Control.Monad.Yaftee.Pipe.ByteString.OnDemand qualified as OnDemand
import Control.Monad.Yaftee.Pipe.ByteString.Crc qualified as Crc
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.Fail qualified as Fail
import Control.Monad.Yaftee.IO qualified as IO
import Control.HigherOpenUnion qualified as U
import Data.TypeLevel.List
import Data.HigherFunctor qualified as HFunctor
import Data.Foldable
import Data.Word
import Data.ByteString qualified as BS
import Data.ByteString.ToolsYj qualified as BS
import Data.ByteString.BitArray qualified as BitArray
import Data.Png.Header
import System.IO
import System.Environment

import Control.Monad.Yaftee.Pipe.Png.Decode.Header qualified as Header
import Control.Monad.Yaftee.Pipe.Deflate.Decompress qualified as Deflate
import Control.Monad.Yaftee.Pipe.Zlib.Decompress qualified as Zlib

main :: IO ()
main = do
	fp : _ <- getArgs
	let	processHeader = IO.print
	h <- openFile fp ReadMode
	void . Eff.runM
		. chunkRun @"chunk"
		. OnDemand.run_ @"header"
		. (`State.run` (1 :: Word32, 0 :: Word32))
		. Deflate.run_ @"deflate"
		. Except.run @String
		. Fail.runExc id
		. Pipe.run
		$ PipeBS.hGet 64 h Pipe.=$=
			png "chunk" "header" processHeader Pipe.=$= do
			PipeIO.print'
			IO.print @(Word32, Word32) =<< State.get

chunkRun :: forall nm es i o r . HFunctor.Loose (U.U es) =>
	Eff.E (ChunkStates nm `Append` es) i o r ->
	Eff.E es i o (((), Chunk), Crc.Crc32)
chunkRun = Crc.runCrc32 . (`State.runN` Chunk "IHDR") . OnDemand.run_

type ChunkStates nm = OnDemand.States nm `Append` '[
	State.Named nm Chunk, State.Named nm Crc.Crc32 ]

type ChunkMembers nm es = (
	OnDemand.Members nm es,
	U.Member (State.Named nm Crc.Crc32) es,
	U.Member (State.Named nm Chunk) es )

png :: forall nmcnk nmhdr -> (
	U.Member Pipe.P es,

	ChunkMembers nmcnk es,
	OnDemand.Members nmhdr es,

	Deflate.Members "deflate" es,
	U.Member (State.S (Word32, Word32)) es,

	U.Member (Except.E String) es,
	U.Member Fail.F es,
	U.Base IO.I es
	) =>
	(Header -> Eff.E es (Either BitArray.B BS.ByteString) BS.ByteString ()) ->
	Eff.E es BS.ByteString BS.ByteString ()
png nmcnk nmhdr processHeader =
	void $ OnDemand.onDemand nmcnk Pipe.=$=
	PipeT.checkRight Pipe.=$= Crc.crc32 nmcnk Pipe.=$= do
		State.putN nmcnk $ OnDemand.RequestBytes 8
		IO.print =<< Pipe.await
		doWhile_ $ chunk1 nmcnk 10
	Pipe.=$= do
		Left (ChunkBegin "IHDR") <- Pipe.await
		_ <- PipeT.checkRight Pipe.=$=
			OnDemand.onDemand nmhdr Pipe.=$=
			(Header.read nmhdr processHeader `Except.catch` IO.print @String)
		Left (ChunkEnd "IHDR") <- Pipe.await

		chunkToByteString nmcnk

--				forever $ Pipe.yield =<< Pipe.await
	Pipe.=$= do

		IO.print =<< Pipe.isMore

		IO.print @Chunk =<< State.getN nmcnk

--				bs <- Pipe.await
		bs <- untilIdat nmcnk
		IO.print =<< Pipe.isMore

		IO.print @Chunk =<< State.getN nmcnk

		_ <- OnDemand.onDemandWithInitial "deflate" bs Pipe.=$= do
			Zlib.decompress "deflate"

		IO.print =<< State.getN @Chunk nmcnk
		_ <- forever $ Pipe.yield =<< Pipe.await
		IO.print =<< State.getN @Chunk nmcnk

chunk1 :: forall nm -> (
	U.Member Pipe.P es,
	OnDemand.Members nm es,
	U.Member (State.Named nm Crc.Crc32) es,
	U.Member (Except.E String) es ) =>
	Int -> Eff.E es BS.ByteString (Either ChunkTag BS.ByteString) Bool
chunk1 nm m = do
	State.putN nm $ OnDemand.RequestBytes 4
	n <- BS.toBitsBE <$> Pipe.await

	Crc.resetCrc32 nm
	State.putN nm (OnDemand.RequestBytes 4)
	cn <- Pipe.await
	Pipe.yield . Left $ ChunkBegin cn

	for_ (split m n) \n' -> do
		State.putN nm $ OnDemand.RequestBytes n'
		Pipe.yield =<< Right <$> Pipe.await

	Crc.compCrc32 nm

	crc1 <- State.getN nm

	State.putN nm $ OnDemand.RequestBytes 4
	crc0 <- Crc.byteStringToCrc32BE <$> Pipe.await

	when (Just crc1 /= crc0) $ Except.throw @String "chunk1: CRC32 error"

	Pipe.yield . Left $ ChunkEnd cn

	pure $ cn /= "IEND"
	where
	split n = fix \go -> \case
		0 -> []
		m'	| n < m' -> n : go (m' - n)
			| otherwise -> [m']

data ChunkTag = ChunkBegin BS.ByteString | ChunkEnd BS.ByteString deriving Show

chunkToByteString :: forall nm -> (
	U.Member Pipe.P es,
	U.Member (State.Named nm Chunk) es,
	U.Member (Except.E String) es,
	U.Member Fail.F es
	) =>
	Eff.E es (Either ChunkTag o) o ()
chunkToByteString nm = do
	Left (ChunkBegin c) <- Pipe.await
	case c of
		"IEND" -> do
			Left (ChunkEnd "IEND") <- Pipe.await
			pure ()
		_ -> do	State.putN nm $ Chunk c
			getUntilChunkEnd nm
			chunkToByteString nm

newtype Chunk = Chunk BS.ByteString deriving Show

getUntilChunkEnd :: forall nm -> (
	U.Member Pipe.P es,
	U.Member (State.Named nm Chunk) es,
	U.Member (Except.E String) es
	) =>
	Eff.E es (Either ChunkTag o) o ()
getUntilChunkEnd nm = Pipe.await >>= \case
	Left (ChunkEnd c) -> do
		Chunk c0 <- State.getN nm
		when (c /= c0) $ Except.throw @String
			"ChunkBegin and ChunkEnd must be pair"
	Right bs -> Pipe.yield bs >> getUntilChunkEnd nm
	_ -> Except.throw @String "bad"

untilIdat :: forall nm -> (
	U.Member Pipe.P es,
	U.Member (State.Named nm Chunk) es,
	U.Base IO.I es
	) =>
	Eff.E es BS.ByteString o BS.ByteString
untilIdat nm = do
	IO.print =<< Pipe.isMore
	bs <- Pipe.await
	State.getN nm >>= \case
		Chunk "IDAT" -> do
			pure bs
		c -> do	IO.print c
			IO.print bs
			untilIdat nm
