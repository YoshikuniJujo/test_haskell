{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ExplicitForAll, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
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
import Control.Monad.Yaftee.Pipe.Png.Decode.Chunk qualified as Chunk
import Control.Monad.Yaftee.Pipe.Png.Decode.Header qualified as Header
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.Fail qualified as Fail
import Control.Monad.Yaftee.IO qualified as IO
import Control.HigherOpenUnion qualified as U
import Data.Foldable
import Data.Sequence qualified as Seq
import Data.Word
import Data.Char
import Data.ByteString qualified as BS
import Data.Png
import Data.Png.Header qualified as Header
import Data.Png.Filters
import System.IO
import System.Environment

import Control.Monad.Yaftee.Pipe.Zlib.Decompress qualified as Zlib
import Data.TypeLevel.List
import Data.Zlib qualified as Zlib
import Data.Sequence.BitArray qualified as BitArray
import Data.HigherFunctor qualified as HFunctor

main :: IO ()
main = do
	fp : _ <- getArgs
	h <- openFile fp ReadMode
	void . Eff.runM . Except.run @String . Fail.runExc id
		. pngRun . Pipe.run
		. (`Except.catch` IO.putStrLn) . void
		$ PipeBS.hGet (32 * 32) h Pipe.=$=
			PipeT.convert bsToSeq Pipe.=$=
			pngDecode IO.print IO.print Pipe.=$= PipeIO.print

pngRun :: HFunctor.Loose (U.U es) =>
	Eff.E (States "foobar" `Append` es) i o r -> Eff.E es i o ()
pngRun = void
	. flip (State.runN @"foobar") Header.header0 . Chunk.chunkRun_ @"foobar"
	. OnDemand.run_ @"foobar" . Zlib.run_

type States nm =
	Zlib.States nm `Append`
	OnDemand.States nm `Append`
	Chunk.ChunkStates nm `Append` '[
	State.Named nm Header.Header ]

pngDecode :: (
	U.Member Pipe.P es, Members "foobar" es,
	U.Member (Except.E String) es, U.Member Fail.F es ) =>
	(Header.Header -> Eff.E es (Either BitArray.B (Seq.Seq Word8)) [Word8] ()) ->
	(Zlib.Header -> Eff.E es (Either BitArray.B (Seq.Seq Word8)) (Seq.Seq Word8) r) ->
	Eff.E es (Seq.Seq Word8) [Word8] ()
pngDecode f g = void $ do
		fhdr <- Chunk.readBytes "foobar" 8
		when (fhdr /= fileHeader) $ Except.throw "File header error"
		Chunk.chunk "foobar" 500
	Pipe.=$= do
		_ <- OnDemand.onDemand "foobar" Pipe.=$= Header.read "foobar" f
		rs <- ((+ 1) <$>) . Header.headerToRows <$> State.getN "foobar"
		bs <- untilIdat "foobar"
		OnDemand.onDemandWithInitial "foobar" bs Pipe.=$=
			Zlib.decompress "foobar" g rs Pipe.=$=
			pngUnfilter "foobar"

type Members nm es = (
	Zlib.Members nm es,
	OnDemand.Members nm es,
	Chunk.ChunkMembers nm es,
	U.Member (State.Named nm Header.Header) es )

pngUnfilter :: forall (nm :: Symbol) -> (
	U.Member Pipe.P es,
	U.Member (State.Named nm Header.Header) es,
	U.Member (Except.E String) es ) =>
	Eff.E es (Seq.Seq Word8) [Word8] ()
pngUnfilter nm = void do
	bs <- Pipe.await
	h <- State.getN nm
	let	bpp = Header.headerToBpp h
		rbs = Header.headerToRowBytes h
	bs' <- either Except.throw pure
		$ unfilter bpp (replicate rbs 0) bs
	Pipe.yield bs'
	unfilterAll bpp bs'

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

seqToString :: Seq.Seq Word8 -> String
seqToString = toList . (chr . fromIntegral <$>)

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
