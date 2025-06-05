{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ExplicitForAll, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import GHC.TypeLits
import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.IO qualified as PipeIO
import Control.Monad.Yaftee.Pipe.ByteString.Lazy qualified as PipeLBS
import Control.Monad.Yaftee.Pipe.ByteString.Lazy.OnDemand qualified as OnDemand
import Control.Monad.Yaftee.Pipe.Png.Decode.Header qualified as Header
import Control.Monad.Yaftee.Pipe.Png.Decode.Chunk qualified as Chunk
import Control.Monad.Yaftee.Pipe.Deflate.Decompress qualified as Deflate
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.Fail qualified as Fail
import Control.Monad.Yaftee.IO qualified as IO
import Control.HigherOpenUnion qualified as U
import Data.ByteString.Lazy qualified as LBS
import Data.Png
import Data.Png.Header qualified as Header
import Data.Zlib qualified as Zlib
import System.IO
import System.Environment

import Pipe.Runlength qualified as Runlength
import Pipe.Huffman qualified as Huffman

main :: IO ()
main = do
	fp : _ <- getArgs
	h <- openFile fp ReadMode
	void . Eff.runM
		. Except.run @String . Fail.runExc id
		. Runlength.run_ @_ @"foobar"
		. Huffman.run @"foobar" @Int
		. flip (State.runN @"foobar") Header.header0
		. OnDemand.run_ @"foobar"
		. Chunk.chunkRun_ @"foobar"
		. Pipe.run . (`Except.catch` IO.putStrLn) . void $
		PipeLBS.hGet 64 h Pipe.=$= do
			readFileHeader "foobar"
			Chunk.chunk "foobar" 25
		Pipe.=$= OnDemand.onDemand "foobar" Pipe.=$= do
			Header.read "foobar" IO.print
			State.putN "foobar" $ OnDemand.RequestBytes 2
			IO.print =<< zlibHeader =<< Except.getRight "bad" =<< Pipe.await
			State.putN "foobar" $ OnDemand.RequestBuffer 15
			Deflate.decompress "foobar"
		Pipe.=$= PipeIO.print

readFileHeader :: forall (nm :: Symbol) -> (
	U.Member Pipe.P es,
	U.Member (State.Named nm Chunk.ByteString) es,
	U.Member (State.Named nm Chunk.Crc32) es,
	U.Member (Except.E String) es
	) =>
	Eff.E es LBS.ByteString o ()
readFileHeader nm = do
	ph <- Chunk.readBytes nm 8
	when (ph /= fileHeader) $ Except.throw @String "PNG File header error"

zlibHeader :: (U.Member (Except.E String) es, U.Member Fail.F es) =>
	LBS.ByteString -> Eff.E es i o Zlib.Header
zlibHeader bs = do
	[cmf, flg] <- pure $ LBS.unpack bs
	maybe (Except.throw "Zlib header check bits error")
		pure (Zlib.readHeader cmf flg)
