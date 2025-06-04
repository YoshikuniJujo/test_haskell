{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Monad
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
import Data.TypeLevel.List
import Data.HigherFunctor qualified as HFunctor
import Data.ByteString.Lazy qualified as LBS
import System.IO
import System.Environment

import Control.Monad.Yaftee.Pipe.Gzip.Decompress
import Pipe.Huffman qualified as Huffman
import Pipe.Runlength qualified as RunLength

import Data.Gzip.Header

import Control.Monad.Yaftee.Pipe.Deflate.Decompress

main :: IO ()
main = do
	fp : _ <- getArgs
	h <- openFile fp ReadMode
	void . Eff.runM . Except.run @String . Fail.runExc id
			. gzipRun @"foobar" . Pipe.run $
		(`Except.catch` IO.putStrLn) . void $ PipeLBS.hGet 64 h Pipe.=$=
			gzipDecompress "foobar" IO.print Pipe.=$= PipeIO.print

gzipRun :: forall nm es i o r . HFunctor.Loose (U.U es) =>
	Eff.E (	'[	State.Named nm PipeLBS.Length,
			State.Named nm Crc.Crc32 ] `Append`
		OnDemand.States nm `Append`
		Huffman.States nm Int `Append`
		RunLength.States nm `Append` es ) i o r ->
	Eff.E es i o ()
gzipRun = RunLength.run_
	. Huffman.run . OnDemand.run_ . Crc.runCrc32 . PipeLBS.lengthRun

gzipDecompress :: forall nm -> (
	U.Member Pipe.P es, GzipMembers nm es,
	U.Member (Except.E String) es, U.Member Fail.F es ) =>
	(GzipHeader -> Eff.E es LBS.ByteString LBS.ByteString r) ->
	Eff.E es LBS.ByteString LBS.ByteString ()
gzipDecompress nm f = void $ OnDemand.onDemand nm Pipe.=$= do
	_ <- PipeT.checkRight Pipe.=$= readHeader nm f
	Crc.resetCrc32 nm

	_ <- deflateDecompress nm Pipe.=$=
		PipeT.convert (either (LBS.pack . (: [])) id)
		Pipe.=$= Crc.crc32 nm Pipe.=$= PipeLBS.length nm

	Crc.compCrc32 nm

	crc <- State.getN nm
	ln <- State.getN nm

	State.putN nm $ OnDemand.RequestBytes 4
	Just crc0 <- Crc.crc32FromByteString <$> PipeT.skipLeft1
	Just ln0 <- PipeLBS.lengthFromByteString 4 <$> (Except.getRight @String "bad 2" =<< Pipe.await)

	when (crc /= crc0) $ Except.throw @String "CRC32 error"
	when (ln /= ln0) $ Except.throw @String "length error"

	Pipe.yield ""

type GzipMembers nm es = (
	RunLength.Members nm es,
	Huffman.Members nm Int es,
	OnDemand.Members nm es,
	U.Member (State.Named nm Crc.Crc32) es,
	U.Member (State.Named nm PipeLBS.Length) es )
