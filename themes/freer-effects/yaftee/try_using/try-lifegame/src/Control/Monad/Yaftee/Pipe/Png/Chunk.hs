{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ExplicitForAll, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.Png.Chunk (
	decode, C(..)
	) where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.ByteString.FingerTree.OnDemand
	qualified as OnDemand
import Control.Monad.Yaftee.Pipe.ByteString.FingerTree.Crc32
	qualified as PipeCrc32
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.HigherOpenUnion qualified as U
import Data.Foldable
import Data.Word
import Data.Word.Word8 qualified as Word8
import Data.Word.Crc32 qualified as Crc32
import Data.ByteString.FingerTree qualified as BSF

decode :: forall nm -> (
	U.Member Pipe.P es, OnDemand.Members nm es,
	U.Member (State.Named nm Crc32.C) es, U.Member (Except.E String) es ) =>
	Eff.E es BSF.ByteString C ()
decode nm = void $ OnDemand.onDemand nm Pipe.=$= PipeCrc32.crc32 nm Pipe.=$= do
	State.putN nm $ OnDemand.RequestBytes 8
	fh <- Pipe.await
	when (fh /= fileHeader) $ Except.throw @String "Not PNG file"
	chunks nm 50

chunks :: forall nm -> (
	U.Member Pipe.P es, OnDemand.Members nm es,
	U.Member (State.Named nm Crc32.C) es, U.Member (Except.E String) es ) =>
	Int -> Eff.E es BSF.ByteString C ()
chunks nm d = fix \go -> do
	b <- chunk1 nm d
	when b go

chunk1 :: forall nm -> (
	U.Member Pipe.P es, OnDemand.Members nm es,
	U.Member (State.Named nm Crc32.C) es, U.Member (Except.E String) es ) =>
	Int -> Eff.E es BSF.ByteString C Bool
chunk1 nm d = do
	State.putN nm $ OnDemand.RequestBytes 4
	n <- Word8.toBitsBE @_ @Int <$> Pipe.await
	PipeCrc32.reset nm
	State.putN nm $ OnDemand.RequestBytes 4
	cn <- Pipe.await
	Pipe.yield $ Begin Nothing cn
	for_ (split d n) \n' -> do
		State.putN nm $ OnDemand.RequestBytes n'
		Pipe.yield . Body =<< Pipe.await
	PipeCrc32.complement nm
	c1 <- State.getN @Crc32.C nm
	State.putN nm $ OnDemand.RequestBytes 4
	c0 <- Crc32.fromWord . Word8.toBitsBE @_ @Word32 <$> Pipe.await
	when (c1 /= c0) $ Except.throw @String "corrupted -- crc32 mismatch"
	Pipe.yield End
	pure $ cn /= "IEND"
	where
	split n = fix \go -> \case
		0 -> []
		m | n < m -> n : go (m - n) | otherwise -> [m]

data C	= Begin (Maybe Int) BSF.ByteString
	| Body BSF.ByteString | End deriving Show

fileHeader :: BSF.ByteString
fileHeader = "\137PNG\r\n\SUB\n"
