{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ExplicitForAll, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

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
import Data.Sequence.Word8 qualified as Seq
import Data.Word
import Data.Char
import Data.ByteString qualified as BS
import Data.Word.Adler32 qualified as Adler32
import Data.Png
import Data.Png.Header qualified as Header
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
					OnDemand.onDemandWithInitial "foobar" bs Pipe.=$= do
						State.putN "foobar" $ OnDemand.RequestBytes 2
						IO.print =<< zlibHeader =<< Except.getRight @String "bad" =<< Pipe.await
						State.putN "foobar" $ OnDemand.RequestBuffer 500
						Deflate.decompress "foobar" Pipe.=$=
							PipeT.convert (either Seq.singleton id) Pipe.=$=
							PipeAdler32.adler32 "foobar" Pipe.=$= PipeIO.print
--						Deflate.decompress "foobar" Pipe.=$= PipeIO.print
						State.putN "foobar" $ OnDemand.RequestBytes 4
						IO.print . snd =<< State.getN @(Int, Adler32.A) "foobar"
--						IO.print . Adler32.fromWord32 . Seq.toBitsBE =<< PipeT.skipLeft1
						IO.print . Adler32.toWord32 . snd =<< State.getN @(Int, Adler32.A) "foobar"
						IO.print @Word32 . Seq.toBitsBE =<< PipeT.skipLeft1
						{-
						forever do
							IO.print =<< Pipe.await
							IO.print @Chunk.Chunk =<< State.getN "foobar"
							-}

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
