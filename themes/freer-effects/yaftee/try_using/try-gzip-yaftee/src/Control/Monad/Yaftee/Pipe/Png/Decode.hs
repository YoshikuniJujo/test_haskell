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

module Control.Monad.Yaftee.Pipe.Png.Decode (

	-- * PNG

	run, States,
	decode, decodeHeader, Members,

	readPngHeader,

	-- * CHUNK

	chunkRun_, ChunkStates,
	chunk1, ChunkMembers,

	Chunk(..)

	) where

import GHC.TypeLits
import Control.Monad
import Control.Monad.Fix
import Control.Monad.ToolsYj
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.ByteString.OnDemand qualified as OnDemand
import Control.Monad.Yaftee.Pipe.ByteString.Crc qualified as Crc
import Control.Monad.Yaftee.Pipe.ByteString.Adler32 qualified as Adler32
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.Fail qualified as Fail
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

import Control.Monad.Yaftee.Pipe.Png.Decode.Header qualified as Header
import Control.Monad.Yaftee.Pipe.Deflate.Decompress qualified as Deflate
import Control.Monad.Yaftee.Pipe.Zlib.Decompress qualified as Zlib

import Pipe.Huffman qualified as Huffman

run :: forall nm es i o r . HFunctor.Loose (U.U es) =>
	Eff.E (States nm `Append` es) i o r -> Eff.E es i o ()
run = void . chunkRun_ . (`State.runN` header0) . Zlib.runNew_

type States nm =
	Zlib.States' nm `Append`
	'[State.Named nm Header] `Append`
	ChunkStates nm

chunkRun_ :: forall nm es i o r . HFunctor.Loose (U.U es) =>
	Eff.E (ChunkStates nm `Append` es) i o r ->
	Eff.E es i o ()
chunkRun_ = void
	. (`State.runN` Crc32 Crc.initialCrc32)
	. (`State.runN` ByteString "")
	. (`State.runN` Chunk "IHDR")

type ChunkStates nm =
	'[State.Named nm Chunk, State.Named nm ByteString, State.Named nm Crc32]

type ChunkMembers nm es = (
	U.Member (State.Named nm Chunk) es,
	U.Member (State.Named nm ByteString) es,
	U.Member (State.Named nm Crc32) es )

readPngHeader :: forall (nm :: Symbol) -> (
	U.Member Pipe.P es,
	U.Member (State.Named nm ByteString) es,
	U.Member (State.Named nm Crc32) es,
	U.Member (Except.E String) es
	) =>
	Eff.E es BS.ByteString o ()
readPngHeader nm = do
	ph <- readBytes nm 8
	when (ph /= pngHeader) $ Except.throw @String "PNG File header error"

pngHeader :: BS.ByteString
pngHeader = "\x89PNG\r\n\SUB\n"

decode :: forall nm -> (
	U.Member Pipe.P es,

	U.Member (State.S Huffman.Phase) es,
	U.Member (State.S (Huffman.IsLiteral Int)) es,

	Members nm es,

	Deflate.Members' nm es,
	U.Member (State.Named nm Adler32.A) es,

	U.Member (Except.E String) es,
	U.Member Fail.F es ) =>
	(Header -> Eff.E es (Either BitArray.B BS.ByteString) BS.ByteString ()) ->
	Eff.E es BS.ByteString BS.ByteString ()
decode nm processHeader = void $ do
		readPngHeader nm
		doWhile_ $ chunk1 nm 100
	Pipe.=$= do

		_ <- OnDemand.onDemand nm Pipe.=$= Header.read nm processHeader

--		IO.print =<< Pipe.isMore

--		IO.print @Chunk =<< State.getN nm

		bs <- untilIdat nm
--		IO.print =<< Pipe.isMore

--		IO.print @Chunk =<< State.getN nm

		hdr <- State.getN nm
		let	wdt = fromIntegral $ headerWidth hdr
			hgt = fromIntegral $ headerHeight hdr
			bpp = headerToBpp hdr

--		IO.print hdr
--		IO.print bpp
--		IO.print $ headerInterlaceMethod hdr

		case headerInterlaceMethod hdr of
			InterlaceMethodNon ->
				void $ OnDemand.onDemandWithInitial nm bs Pipe.=$= do
					Zlib.decompressNew nm (headerToRowBytes hdr + 1)
			InterlaceMethodAdam7 ->
				void $ OnDemand.onDemandWithInitial nm bs Pipe.=$= do
					Zlib.decompressNew' nm wdt hgt bpp
			_ -> Except.throw @String "no such interlace methods"

--		IO.print =<< State.getN @Chunk nm
--		_ <- forever $ Pipe.yield =<< Pipe.await
--		IO.print =<< State.getN @Chunk nm

	Pipe.=$= do
		bs <- Pipe.await
--		IO.print @String "foobar"
		h <- State.getN nm
		let	bpp = headerToBpp h
			rbs = headerToRowBytes h
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

decodeHeader :: forall nm ->
	(U.Member Pipe.P es, Members nm es, U.Member (Except.E String) es ) =>
	(Header -> Eff.E es (Either BitArray.B BS.ByteString) BS.ByteString ()) ->
	Eff.E es BS.ByteString BS.ByteString ()
decodeHeader nm processHeader =
	void $ do
		readPngHeader nm
		doWhile_ $ chunk1 nm 100
	Pipe.=$= OnDemand.onDemand nm Pipe.=$= Header.read nm processHeader

type Members nm es = (
	U.Member (State.Named nm Header) es,
	ChunkMembers nm es,
	OnDemand.Members nm es )

chunk1 :: forall (nm :: Symbol) -> (
	U.Member Pipe.P es,
	ChunkMembers nm es,
	U.Member (Except.E String) es
	) =>
	Int -> Eff.E es BS.ByteString BS.ByteString Bool
chunk1 nm m = do
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
	when (Just crc1 /= crc0) $ Except.throw @String "chunk1: CRC32 error"
--	Pipe.yield . Left $ ChunkEnd cn
	pure $ cn /= "IEND"
	where
	split n = fix \go -> \case
		0 -> []
		m'	| n < m' -> n : go (m' - n)
			| otherwise -> [m']

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

untilIdat :: forall nm -> (
	U.Member Pipe.P es,
	U.Member (State.Named nm Chunk) es
	) =>
	Eff.E es BS.ByteString o BS.ByteString
untilIdat nm = do
	bs <- Pipe.await
	State.getN nm >>= \case
		Chunk "IDAT" -> do
			pure bs
		c -> do
			untilIdat nm
