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
import Control.Monad.Yaftee.Pipe.ByteString.Lazy qualified as PipeLBS
import Control.Monad.Yaftee.Pipe.ByteString.Lazy.OnDemand qualified as OnDemand
import Control.Monad.Yaftee.Pipe.Png.Decode.Header qualified as Header
import Control.Monad.Yaftee.Pipe.Png.Decode.Chunk qualified as Chunk
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.IO qualified as IO
import Control.HigherOpenUnion qualified as U
import Data.ByteString.Lazy qualified as LBS
import Data.Png
import Data.Png.Header qualified as Header
import System.IO
import System.Environment

main :: IO ()
main = do
	fp : _ <- getArgs
	h <- openFile fp ReadMode
	void . Eff.runM
		. flip (State.runN @"foobar") Header.header0
		. OnDemand.run_ @"foobar"
		. Chunk.chunkRun_ @"foobar"
		. Except.run @String . Pipe.run . (`Except.catch` IO.putStrLn) . void $
		PipeLBS.hGet 64 h Pipe.=$= do
			readFileHeader "foobar"
			Chunk.chunk "foobar" 25
		Pipe.=$= OnDemand.onDemand "foobar" Pipe.=$= do
			Header.read "foobar" IO.print
			State.putN "foobar" $ OnDemand.RequestBytes 2
			IO.print =<< Pipe.await
			State.putN "foobar" $ OnDemand.RequestBuffer 15
			forever do
				IO.print =<< Pipe.await
				IO.print @Chunk.Chunk =<< State.getN "foobar"

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
