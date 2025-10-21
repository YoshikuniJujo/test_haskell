{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Monad
import Control.Monad.ToolsYj
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeBS
import Control.Monad.Yaftee.Pipe.BytesCrc32 qualified as Bytes
import Control.Monad.Yaftee.Pipe.PngNg.Decode.Chunk
import Control.Monad.Yaftee.Pipe.Png.Encode.Chunk qualified as Chunk
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.Fail qualified as Fail
import Control.Monad.Yaftee.IO qualified as IO
import Control.HigherOpenUnion qualified as U
import Data.Ratio
import Data.Word
import Data.ByteString.FingerTree qualified as BSF
import Data.Apng qualified as Apng
import System.IO
import System.Environment

main :: IO ()
main = do
	fp : fpo : _ <- getArgs
	h <- openFile fp ReadMode; ho <- openFile fpo WriteMode
	void . Eff.runM
		. Bytes.bytesRun_ @"foobar"
		. flip (State.runN @"foobar") ("" :: BSF.ByteString)
		. Except.run @String . Fail.run . Pipe.run
		. (`Fail.catch` IO.putStrLn) . (`Except.catch` IO.putStrLn)
		. void $ PipeBS.hGet 32 h
		Pipe.=$= PipeT.convert BSF.fromStrict
		Pipe.=$= chunks "foobar"
		Pipe.=$= do
			ChunkBegin "IHDR" <- Pipe.await
			bd <- chunkBody "foobar"
			Pipe.yield \() -> (Chunk.Chunk "IHDR" bd, ())

			doWhile_ do
				ChunkBegin cnm <- Pipe.await
				case cnm of
					"IDAT" -> do
						bd <- chunkBody "foobar"
						Pipe.yield \() -> (Chunk.Chunk cnm bd, ())
						pure True
					"IEND" -> pure False
					_ -> pure True

			Pipe.yield \() -> (Chunk.Chunk "IEND" "", ())
		Pipe.=$= Chunk.chunks ()
		Pipe.=$= PipeT.convert BSF.toStrict
		Pipe.=$= PipeBS.hPutStr ho
	hClose h; hClose ho

chunkBody :: forall nm -> (
	U.Member Pipe.P es, U.Member (State.Named nm BSF.ByteString) es,
	U.Member (Except.E String) es ) => Eff.E es Chunk o BSF.ByteString
chunkBody nm = Pipe.await >>= \case
	ChunkBody bd -> State.modifyN nm (<> bd) >> chunkBody nm
	ChunkEnd -> State.getN nm <* State.putN @BSF.ByteString nm ""
	_ -> Except.throw @String "chunkBody: not ChunkBody"

-- header :: Word32 -> Word32 ->

actl :: Word32 -> Apng.Actl
actl fn = Apng.Actl { Apng.actlFrames = fn, Apng.actlPlays = 1 }

fctl :: Word32 -> Word32 -> Ratio Word16 -> Apng.Fctl
fctl w h d = Apng.Fctl {
	Apng.fctlSequenceNumber = 0,
	Apng.fctlWidth = w, Apng.fctlHeight = h,
	Apng.fctlXOffset = 0, Apng.fctlYOffset = 0,
	Apng.fctlDelayNum = numerator d, Apng.fctlDelayDen = denominator d,
	Apng.fctlDisposeOp = Apng.disposeOpNone,
	Apng.fctlBlendOp = Apng.blendOpSource }
