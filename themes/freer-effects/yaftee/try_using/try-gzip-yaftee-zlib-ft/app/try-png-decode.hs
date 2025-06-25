{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Prelude hiding (Monoid)
import Foreign.C.ByteArray qualified as CByteArray
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.IO qualified as PipeIO
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeBS
import Control.Monad.Yaftee.Pipe.ByteString.FingerTree.OnDemand qualified as OnDemand
import Control.Monad.Yaftee.Pipe.Png.Decode.Header qualified as Header
import Control.Monad.Yaftee.Pipe.Png.Decode.Chunk qualified as Chunk
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.IO qualified as IO
import Control.HigherOpenUnion qualified as U
import Data.ByteString.FingerTree qualified as BSF
import Data.Png
import Data.Png.Header qualified as Header
import System.IO
import System.Environment

import Control.Monad.Yaftee.Pipe.Zlib qualified as PipeZ

import Codec.Compression.Zlib.Advanced.Core qualified as Zlib
import Codec.Compression.Zlib.Constant.Core qualified as Zlib

main :: IO ()
main = do
	fp : _ <- getArgs
	h <- openFile fp ReadMode
	ib <- CByteArray.malloc 64
	ob <- CByteArray.malloc 64
	void . Eff.runM . Except.run @String . Except.run @Zlib.ReturnCode
		. flip (State.runN @"foobar") (Monoid ("" :: BSF.ByteString))
		. PipeZ.inflateRun @"foobar"
		. Chunk.chunkRun_ @"foobar"
		. OnDemand.run @"foobar"
		. flip (State.runN @"foobar") Header.header0
		. Pipe.run
		$ PipeBS.hGet 64 h Pipe.=$=
			PipeT.convert BSF.fromStrict Pipe.=$= do
				fhdr <- Chunk.readBytes "foobar" 8
				IO.print fhdr
				Chunk.chunk "foobar" 500
			Pipe.=$= forever do
				x <- Pipe.await
				c <- State.getN "foobar"
				when (c == Chunk.Chunk "IHDR" || c == Chunk.Chunk "IDAT") $ Pipe.yield x
			Pipe.=$= do
				OnDemand.onDemand "foobar" Pipe.=$= Header.read "foobar" IO.print
				PipeZ.inflate "foobar" IO (Zlib.WindowBitsZlib 15) ib ob
			Pipe.=$= do
				bs0 <- Pipe.await
				rs <- ((+ 1) <$>) . Header.headerToRows <$> State.getN "foobar"
				IO.print rs
				format "foobar" bs0 rs
			Pipe.=$= PipeIO.print
	CByteArray.free ib
	CByteArray.free ob

format :: forall nm -> (
	U.Member Pipe.P es,
	U.Member (State.Named nm (Monoid BSF.ByteString)) es ) =>
	BSF.ByteString -> [Int] -> Eff.E es BSF.ByteString BSF.ByteString ()
format nm bs0 ns0 = do
	State.putN nm $ Monoid bs0
	($ ns0) $ fix \go -> \case
		[] -> pure ()
		n : ns -> do
			Pipe.yield =<< getInput nm n
			go ns

getInput :: forall nm -> (
	U.Member Pipe.P es,
	U.Member (State.Named nm (Monoid BSF.ByteString)) es ) =>
	Int -> Eff.E es BSF.ByteString o BSF.ByteString
getInput nm n = State.getN nm >>= \(Monoid bs) -> case BSF.splitAt' n bs of
	Nothing -> readMore nm >> getInput nm n
	Just (t, d) -> t <$ State.putN nm (Monoid d)

readMore :: forall nm -> (
	Semigroup mono, U.Member Pipe.P es,
	U.Member (State.Named nm (Monoid mono)) es ) =>
	Eff.E es mono o ()
readMore nm =
	Pipe.await >>= \xs -> State.modifyN nm (Monoid . (<> xs) . unMonoid)

newtype Monoid m = Monoid { unMonoid :: m } deriving Show
