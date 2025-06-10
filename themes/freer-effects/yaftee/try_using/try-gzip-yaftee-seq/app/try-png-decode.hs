{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ExplicitForAll, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import GHC.TypeLits
import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.IO qualified as PipeIO
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeBS
import Control.Monad.Yaftee.Pipe.BitArray.OnDemand qualified as OnDemand
import Control.Monad.Yaftee.Pipe.Sequence.Adler32 qualified as PipeAdler32
import Control.Monad.Yaftee.Pipe.Png.Decode.Chunk qualified as Chunk
import Control.Monad.Yaftee.Pipe.Png.Decode.Header qualified as Header
import Control.Monad.Yaftee.Pipe.Deflate.Decompress qualified as Deflate
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.Fail qualified as Fail
import Control.Monad.Yaftee.IO qualified as IO
import Control.HigherOpenUnion qualified as U
import Data.Foldable
import Data.Sequence qualified as Seq
import Data.Sequence.ToolsYj qualified as Seq
import Data.Sequence.Word8 qualified as Seq
import Data.Sequence.BitArray qualified as BitArray
import Data.Word
import Data.Char
import Data.ByteString qualified as BS
import Data.Word.Adler32 qualified as Adler32
import Data.Png
import Data.Png.Header qualified as Header
import Data.Png.Filters
import Data.Zlib qualified as Zlib
import System.IO
import System.Environment

main :: IO ()
main = do
	fp : _ <- getArgs
	h <- openFile fp ReadMode
	void . Eff.runM . Except.run @String . Fail.runExc id
		. Deflate.run_ @"foobar"
		. OnDemand.run_ @"foobar"
		. flip (State.runN @"foobar") (Sequence Seq.empty)
		. flip (State.runN @"foobar") Adler32.initial
		. flip (State.runN @"foobar") Header.header0
		. Chunk.chunkRun_ @"foobar" . Pipe.run
		. (`Except.catch` IO.putStrLn) . void
		$ PipeBS.hGet (32 * 32) h Pipe.=$=
			PipeT.convert bsToSeq Pipe.=$= do
					fhdr <- Chunk.readBytes "foobar" 8
					when (fhdr /= fileHeader) $ Except.throw "File header error"
					Chunk.chunk "foobar" 500
				Pipe.=$= do
					_ <- OnDemand.onDemand "foobar" Pipe.=$= Header.read "foobar" IO.print
					bs <- untilIdat "foobar"
					OnDemand.onDemandWithInitial "foobar" bs Pipe.=$= -- do
						zlibDecompress IO.print Pipe.=$= pngUnfilter "foobar"

zlibDecompress :: (
	U.Member Pipe.P es,
	Deflate.Members "foobar" es,
	U.Member (State.Named "foobar" Header.Header) es,
	U.Member (State.Named "foobar" Sequence) es,
	U.Member (State.Named "foobar" (Int, Adler32.A)) es,
	U.Member (Except.E String) es, U.Member Fail.F es) =>
	(Zlib.Header ->
		Eff.E es (Either BitArray.B (Seq.Seq Word8)) (Seq.Seq Word8) r) ->
	Eff.E es (Either BitArray.B (Seq.Seq Word8)) (Seq.Seq Word8) ()
zlibDecompress f = do
	State.putN "foobar" $ OnDemand.RequestBytes 2
	f =<< zlibHeader =<< Except.getRight @String "bad" =<< Pipe.await
	State.putN "foobar" $ OnDemand.RequestBuffer 500
	rs <- ((+ 1) <$>) . Header.headerToRows <$> State.getN "foobar"
	bpp <- Header.headerToBpp <$> State.getN "foobar"
	rbs <- Header.headerToRowBytes <$> State.getN "foobar"
	Deflate.decompress "foobar" Pipe.=$=
		format "foobar" rs Pipe.=$= PipeAdler32.adler32 "foobar"
	State.putN "foobar" $ OnDemand.RequestBytes 4
	adl1 <- Adler32.toWord32 . snd <$> State.getN @(Int, Adler32.A) "foobar"
	adl0 <- Seq.toBitsBE <$> PipeT.skipLeft1
	when (adl1 /= adl0) $ Except.throw @String "ADLER-32 error"

pngUnfilter :: forall (nm :: Symbol) -> (
	U.Member Pipe.P es,
	U.Member (State.Named nm Header.Header) es,
	U.Member (Except.E String) es,
	U.Base IO.I es
	) =>
	Eff.E es (Seq.Seq Word8) o ()
pngUnfilter nm = void do
	bs <- Pipe.await
	h <- State.getN nm
	let	bpp = Header.headerToBpp h
		rbs = Header.headerToRowBytes h
	bs' <- either Except.throw pure
		$ unfilter bpp (replicate rbs 0) bs
	IO.print bs'
	unfilterAll bpp bs' Pipe.=$= PipeIO.print

bsToSeq :: BS.ByteString -> Seq.Seq Word8
bsToSeq = Seq.fromList . BS.unpack

untilIdat :: forall nm -> (
	U.Member Pipe.P es,
	U.Member (State.Named nm Chunk.Chunk) es
	) =>
	Eff.E es (Seq.Seq Word8) o (Seq.Seq Word8)
untilIdat nm = do
	bs <- Pipe.await
	State.getN nm >>= \case
		Chunk.Chunk (seqToString -> "IDAT") -> do
			pure bs
		c -> do
			untilIdat nm

seqFromString :: String -> Seq.Seq Word8
seqFromString = Seq.fromList . (fromIntegral . ord <$>)

seqToString :: Seq.Seq Word8 -> String
seqToString = toList . (chr . fromIntegral <$>)

zlibHeader :: (U.Member (Except.E String) es, U.Member Fail.F es) =>
	Seq.Seq Word8 -> Eff.E es i o Zlib.Header
zlibHeader bs = do
	[cmf, flg] <- pure $ toList bs
	maybe (Except.throw @String "Zlib header check bits error")
		pure (Zlib.readHeader cmf flg)

newtype Sequence = Sequence { unSequence :: Seq.Seq Word8 } deriving Show

format :: forall (nm :: Symbol) -> (
	U.Member Pipe.P es,
	U.Member (State.Named nm Sequence) es,
	U.Member Fail.F es ) =>
	[Int] -> Eff.E es (Either Word8 (Seq.Seq Word8)) (Seq.Seq Word8) ()
format nm = \case
	[] -> do { Right Seq.Empty <- Pipe.await; pure () }
	r : rs -> (Pipe.yield =<< getBytes nm r) >> format nm rs

getBytes :: forall (nm :: Symbol) ->
	(U.Member Pipe.P es, U.Member (State.Named nm Sequence) es) =>
	Int -> Eff.E es (Either Word8 (Seq.Seq Word8)) o (Seq.Seq Word8)
getBytes nm n = State.getsN nm (Seq.splitAt' n . unSequence) >>= \case
	Nothing -> readMore nm >> getBytes nm n
	Just (t, d) -> t <$ State.putN nm (Sequence d)

readMore :: forall (nm :: Symbol) ->
	(U.Member Pipe.P es, U.Member (State.Named nm Sequence) es) =>
	Eff.E es (Either Word8 (Seq.Seq Word8)) o ()
readMore nm = Pipe.await >>= \case
	Left w -> State.modifyN nm $ Sequence . (Seq.:|> w) . unSequence
	Right s -> State.modifyN nm $ Sequence . (Seq.>< s) . unSequence

unfilterAll :: (
	U.Member Pipe.P es,
	U.Member (Except.E String) es
	) =>
	Int -> [Word8] -> Eff.E es (Seq.Seq Word8) [Word8] ()
unfilterAll bpp prior = do
	mbs <- Pipe.awaitMaybe
	case mbs of
		Nothing -> pure ()
		Just Seq.Empty -> pure ()
		Just bs -> do
			bs' <- either Except.throw pure $ unfilter bpp prior bs
			Pipe.yield bs'
			unfilterAll bpp bs'
