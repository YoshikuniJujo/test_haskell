{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.Png.DecodeNew (

	pngRun, PngStates,
	png, PngMembers,

	pngHeader,

	pngRunNew, PngStates',
	png',

	) where

import GHC.TypeLits
import Control.Monad
import Control.Monad.Fix
import Control.Monad.ToolsYj
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.ByteString.OnDemand qualified as OnDemand
import Control.Monad.Yaftee.Pipe.ByteString.Crc qualified as Crc
import Control.Monad.Yaftee.Pipe.ByteString.Adler32 qualified as Adler32
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.Fail qualified as Fail
import Control.Monad.Yaftee.IO qualified as IO
import Control.HigherOpenUnion qualified as U
import Data.TypeLevel.List
import Data.HigherFunctor qualified as HFunctor
import Data.Foldable
import Data.Bool
import Data.ByteString qualified as BS
import Data.ByteString.ToolsYj qualified as BS
import Data.ByteString.BitArray qualified as BitArray
import Data.Png.Header
import Data.Png.Filters

import Control.Monad.Yaftee.Pipe.Png.Decode.HeaderNew qualified as Header
import Control.Monad.Yaftee.Pipe.Deflate.Decompress qualified as Deflate
import Control.Monad.Yaftee.Pipe.Zlib.Decompress qualified as Zlib

import Pipe.Huffman qualified as Huffman

pngRun :: forall nmcnk nmhdr es i o r . HFunctor.Loose (U.U es) =>
	Eff.E (PngStates nmcnk nmhdr `Append` es) i o r -> Eff.E es i o ()
pngRun = void . (`State.runN` header0) . chunkRun . Zlib.run_

pngRunNew :: forall nmcnk nmhdr es i o r . HFunctor.Loose (U.U es) =>
	Eff.E (PngStates' nmcnk nmhdr `Append` es) i o r -> Eff.E es i o ()
pngRunNew = void
	. (`State.runN` Crc32 Crc.initialCrc32)
	. (`State.runN` ByteString "")
	. (`State.runN` header0) . chunkRun' . Zlib.runNew_

type PngStates nmcnk nmhdr =
	Zlib.States nmhdr `Append` ChunkStates nmcnk `Append` '[State.Named nmhdr Header]

type PngStates' nmcnk nmhdr =
	Zlib.States' nmhdr `Append` ChunkStates' nmcnk `Append` '[
		State.Named nmhdr Header, State.Named nmcnk ByteString, State.Named nmcnk Crc32 ]

chunkRun :: forall nm es i o r . HFunctor.Loose (U.U es) =>
	Eff.E (ChunkStates nm `Append` es) i o r ->
	Eff.E es i o (((), Chunk), Crc.Crc32)
chunkRun = Crc.runCrc32 . (`State.runN` Chunk "IHDR") . OnDemand.run_

type ChunkStates nm = OnDemand.States nm `Append` '[
	State.Named nm Chunk, State.Named nm Crc.Crc32 ]

chunkRun' :: forall nm es i o r . HFunctor.Loose (U.U es) =>
	Eff.E (ChunkStates' nm `Append` es) i o r ->
	Eff.E es i o (r, Chunk)
chunkRun' = (`State.runN` Chunk "IHDR")

type ChunkStates' nm = '[State.Named nm Chunk]

type ChunkMembers nm es = (
	OnDemand.Members nm es,
	U.Member (State.Named nm Crc.Crc32) es,
	U.Member (State.Named nm Chunk) es )

type ChunkMembers' nm es = (
	U.Member (State.Named nm Chunk) es )

png :: forall nmcnk nmhdr -> (
	U.Member Pipe.P es,

	PngMembers nmcnk nmhdr es,

	Deflate.Members nmhdr es,
	U.Member (State.Named nmhdr Adler32.A) es,

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
		doWhile_ $ chunk1 nmcnk 1000
	Pipe.=$= do

		chunkToByteString nmcnk

--				forever $ Pipe.yield =<< Pipe.await
	Pipe.=$= do

--		Left (ChunkBegin "IHDR") <- Pipe.await
		_ <- OnDemand.onDemand nmhdr Pipe.=$=
			(Header.read nmhdr processHeader `Except.catch` IO.print @String)
--		Left (ChunkEnd "IHDR") <- Pipe.await

		IO.print =<< Pipe.isMore

		IO.print @Chunk =<< State.getN nmcnk

--				bs <- Pipe.await
		bs <- untilIdat nmcnk
		IO.print =<< Pipe.isMore

		IO.print @Chunk =<< State.getN nmcnk

		hdr <- State.getN nmhdr
		let	wdt = fromIntegral $ headerWidth hdr
			hgt = fromIntegral $ headerHeight hdr
			bpp = headerToBpp hdr

		IO.print hdr
		IO.print bpp
		IO.print $ headerInterlaceMethod hdr

		case headerInterlaceMethod hdr of
			InterlaceMethodNon ->
				void $ OnDemand.onDemandWithInitial nmhdr bs Pipe.=$= do
					Zlib.decompress nmhdr (headerToRowBytes hdr + 1)
			InterlaceMethodAdam7 ->
				void $ OnDemand.onDemandWithInitial nmhdr bs Pipe.=$= do
					Zlib.decompress' nmhdr wdt hgt bpp
			_ -> Except.throw @String "no such interlace methods"

		IO.print =<< State.getN @Chunk nmcnk
		_ <- forever $ Pipe.yield =<< Pipe.await
		IO.print =<< State.getN @Chunk nmcnk

	Pipe.=$= do
		bs <- Pipe.await
		IO.print @String "foobar"
		h <- State.getN nmhdr
		let	wdt = headerWidth h
			hgt = headerHeight h
			dpt = headerBitDepth h
			ct = headerColorType h
			bpp = headerToBpp h
			rbs = headerToRowBytes h
		IO.print wdt
		IO.print hgt
		IO.print dpt
		IO.print ct
		IO.print bpp
		IO.print rbs
		bs' <- either Except.throw pure
			$ unfilter bpp (BS.replicate rbs 0) bs
		Pipe.yield bs'
		unfilterAll bpp bs'
--		forever $ Pipe.yield =<< Pipe.await

png' :: forall nmcnk nmhdr -> (
	U.Member Pipe.P es,

	U.Member (State.Named nmcnk ByteString) es,
	U.Member (State.Named nmcnk Crc32) es,

	U.Member (State.S Huffman.Phase) es,
	U.Member (State.S (Huffman.IsLiteral Int)) es,

	PngMembers' nmcnk nmhdr es,

	Deflate.Members' nmhdr es,
	U.Member (State.Named nmhdr Adler32.A) es,

	U.Member (Except.E String) es,
	U.Member Fail.F es,
	U.Base IO.I es
	) =>
	(Header -> Eff.E es (Either BitArray.B BS.ByteString) BS.ByteString ()) ->
	Eff.E es BS.ByteString BS.ByteString ()
png' nmcnk nmhdr processHeader = void $ do
		IO.print =<< readBytes nmcnk 8
		doWhile_ $ chunk1' nmcnk 100
	Pipe.=$= do

		_ <- OnDemand.onDemand nmhdr Pipe.=$=
			(Header.read nmhdr processHeader `Except.catch` IO.print @String)

		IO.print =<< Pipe.isMore

		IO.print @Chunk =<< State.getN nmcnk

--				bs <- Pipe.await
		bs <- untilIdat nmcnk
		IO.print =<< Pipe.isMore

		IO.print @Chunk =<< State.getN nmcnk

		hdr <- State.getN nmhdr
		let	wdt = fromIntegral $ headerWidth hdr
			hgt = fromIntegral $ headerHeight hdr
			bpp = headerToBpp hdr

		IO.print hdr
		IO.print bpp
		IO.print $ headerInterlaceMethod hdr

		case headerInterlaceMethod hdr of
			InterlaceMethodNon ->
				void $ OnDemand.onDemandWithInitial nmhdr bs Pipe.=$= do
					Zlib.decompressNew nmhdr (headerToRowBytes hdr + 1)
			InterlaceMethodAdam7 ->
				void $ OnDemand.onDemandWithInitial nmhdr bs Pipe.=$= do
					Zlib.decompressNew' nmhdr wdt hgt bpp
			_ -> Except.throw @String "no such interlace methods"

		IO.print =<< State.getN @Chunk nmcnk
		_ <- forever $ Pipe.yield =<< Pipe.await
		IO.print =<< State.getN @Chunk nmcnk

	Pipe.=$= do
		bs <- Pipe.await
		IO.print @String "foobar"
		h <- State.getN nmhdr
		let	wdt = headerWidth h
			hgt = headerHeight h
			dpt = headerBitDepth h
			ct = headerColorType h
			bpp = headerToBpp h
			rbs = headerToRowBytes h
		IO.print wdt
		IO.print hgt
		IO.print dpt
		IO.print ct
		IO.print bpp
		IO.print rbs
		bs' <- either Except.throw pure
			$ unfilter bpp (BS.replicate rbs 0) bs
		Pipe.yield bs'
		unfilterAll bpp bs'
--		forever $ Pipe.yield =<< Pipe.await

unfilterAll :: (
	U.Member Pipe.P es,
	U.Member (Except.E String) es
	) =>
	Int -> BS.ByteString -> Eff.E es BS.ByteString BS.ByteString ()
unfilterAll bpp prior = do
	mbs <- Pipe.awaitMaybe
	case mbs of
		Nothing -> pure ()
		Just "" -> pure ()
		Just bs -> do
			bs' <- either Except.throw pure $ unfilter bpp prior bs
			Pipe.yield bs'
			unfilterAll bpp bs'

pngHeader :: forall nmcnk nmhdr -> (
	U.Member Pipe.P es,

	PngMembers nmcnk nmhdr es,

	U.Member (Except.E String) es,
	U.Member Fail.F es,
	U.Base IO.I es
	) =>
	(Header -> Eff.E es (Either BitArray.B BS.ByteString) BS.ByteString ()) ->
	Eff.E es BS.ByteString BS.ByteString ()
pngHeader nmcnk nmhdr processHeader =
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
		pure ()

type PngMembers nmcnk nmhdr es = (
	U.Member (State.Named nmhdr Header) es,
	ChunkMembers nmcnk es,
	OnDemand.Members nmhdr es )

type PngMembers' nmcnk nmhdr es = (
	U.Member (State.Named nmhdr Header) es,
	ChunkMembers' nmcnk es,
	OnDemand.Members nmhdr es )

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

chunk1' :: forall (nm :: Symbol) -> (
	U.Member Pipe.P es,
	U.Member (State.Named nm Chunk) es,
	U.Member (State.Named nm ByteString) es,
	U.Member (State.Named nm Crc32) es,
	U.Member (Except.E String) es
	) =>
	Int -> Eff.E es BS.ByteString BS.ByteString Bool
chunk1' nm m = do
	n <- BS.toBitsBE <$> readBytes nm 4
	resetCrc32 nm
	cn <- readBytes nm 4
	State.putN nm $ Chunk cn
--	Pipe.yield . Left $ ChunkBegin cn
--	for_ (split m n) \n' -> Pipe.yield =<< Right <$> readBytes nm n'
	for_ (split m n) \n' -> Pipe.yield =<< readBytes nm n'
	compCrc32 nm
	Crc32 crc1 <- State.getN nm
	crc0 <- Crc.byteStringToCrc32BE <$> readBytes nm 4
	when (Just crc1 /= crc0) $ Except.throw @String "chunk1': CRC32 error"
--	Pipe.yield . Left $ ChunkEnd cn
	pure $ cn /= "IEND"
	where
	split n = fix \go -> \case
		0 -> []
		m'	| n < m' -> n : go (m' - n)
			| otherwise -> [m']

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

readBytes :: forall (nm :: Symbol) -> (
	U.Member Pipe.P es,
	U.Member (State.Named nm ByteString) es,
	U.Member (State.Named nm Crc32) es,
	U.Member (Except.E String) es ) =>
	Int -> Eff.E es BS.ByteString o BS.ByteString
readBytes nm n = State.getsN nm (BS.splitAt' n . unByteString) >>= \case
	Nothing -> readMore nm
		>>= bool (Except.throw @String "no more ByteString") (readBytes nm n)
	Just (t, d) -> t <$ do
		State.modifyN nm $ Crc32 . (`Crc.crc32StepBS'` t) . unCrc32
		State.putN nm (ByteString d)

resetCrc32 :: forall (nm :: Symbol) ->
	(U.Member (State.Named nm Crc32) es) => Eff.E es i o ()
resetCrc32 nm = State.putN nm $ Crc32 Crc.initialCrc32

compCrc32 :: forall (nm :: Symbol) ->
	(U.Member (State.Named nm Crc32) es) => Eff.E es i o ()
compCrc32 nm = State.modifyN nm $ Crc32 . Crc.complementCrc32 . unCrc32

readMore :: forall (nm :: Symbol) -> (
	U.Member Pipe.P es,
	U.Member (State.Named nm ByteString) es ) =>
	Eff.E es BS.ByteString o Bool
readMore nm = Pipe.awaitMaybe >>= \case
	Nothing -> pure False
	Just bs -> True <$ State.modifyN nm (`appendByteString` bs)

newtype ByteString = ByteString { unByteString :: BS.ByteString } deriving Show
newtype Crc32 = Crc32 { unCrc32 :: Crc.Crc32 } deriving Show

appendByteString :: ByteString -> BS.ByteString -> ByteString
appendByteString (ByteString bs1) bs2 = ByteString $ bs1 `BS.append` bs2

data ChunkTag = ChunkBegin BS.ByteString | ChunkEnd BS.ByteString deriving Show

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
