{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Arrow
import Control.Monad
import Control.Monad.ToolsYj
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.IO qualified as PipeIO
import Control.Monad.Yaftee.Pipe.ByteString.Lazy qualified as PipeLBS
import Control.Monad.Yaftee.Pipe.ByteString.Lazy.OnDemand qualified as OnDemand
import Control.Monad.Yaftee.Pipe.ByteString.Lazy.Crc qualified as Crc
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.Fail qualified as Fail
import Control.Monad.Yaftee.IO qualified as IO
import Control.HigherOpenUnion qualified as U
import Data.Bits
import Data.Word
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.ToolsYj qualified as LBS
import Data.ByteString.Lazy.BitArray qualified as BitArray
import System.IO
import System.Environment

import Control.Monad.Yaftee.Pipe.Gzip.Decompress

main :: IO ()
main = do
	fp : _ <- getArgs
	h <- openFile fp ReadMode
	let	f = IO.print
	void . Eff.runM . Except.run @String . Fail.runExc id
			. OnDemand.run_ @"foobar"
			. Crc.runCrc32 @"foobar"
			. Pipe.run $
		(`Except.catch` IO.putStrLn) . void $ PipeLBS.hGet 64 h Pipe.=$=
			OnDemand.onDemand "foobar" Pipe.=$= do
				_ <- PipeT.checkRight Pipe.=$= readHeader "foobar" f

				Crc.resetCrc32 "foobar"
				doWhile_ block Pipe.=$= Crc.crc32 "foobar" Pipe.=$= PipeIO.print
				Crc.compCrc32 "foobar"

				IO.print . Crc.crc32ToByteString =<< State.getN "foobar"

				State.putN "foobar" $ OnDemand.RequestBuffer 100
				PipeIO.print

block :: (
	U.Member Pipe.P es,
	U.Member (State.Named "foobar" OnDemand.Request) es,
	U.Member (Except.E String) es,
	U.Member Fail.F es ) =>
	Eff.E es (Either BitArray.B LBS.ByteString) LBS.ByteString Bool
block = do
	State.putN "foobar" $ OnDemand.RequestBits 1
	Just bf <- either
		(Just . BitArray.toBits @Word8)
		(const Nothing) <$> Pipe.await
	State.putN "foobar" $ OnDemand.RequestBits 2
	Just bt <- either
		(Just . BitArray.toBits @Word8)
		(const Nothing) <$> Pipe.await
	(bf /= 1) <$ case bt of
		0 -> do
			State.putN "foobar" $ OnDemand.RequestBytes 4
			ln <- pairToLength =<< PipeT.skipLeft1
			State.putN "foobar" $ OnDemand.RequestBytes ln
			Pipe.yield =<< Except.getRight "bad" =<< Pipe.await
		_ -> error "yet"

pairToLength bs0 = fromIntegral @Word16 ln <$ do
	when (LBS.length bs0 /= 4) $ Except.throw @String "not 4 bytes"
	when (ln /= complement cln) $ Except.throw @String "bad pair"
	where (ln, cln) = (LBS.toBits *** LBS.toBits) $ LBS.splitAt 2 bs0
