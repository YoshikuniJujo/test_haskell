{-# LANGUAGE ImportQualifiedPost, PackageImports #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.Png.Encode.Chunk (
	chunks, chunksSt, Chunk(..)
	) where

import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.Fail qualified as Fail
import Control.HigherOpenUnion qualified as U
import Data.MonoTraversable
import Data.Word
import Data.Word.Crc32 qualified as Crc32
import Data.ByteString.FingerTree qualified as BSF
import Data.ByteString.FingerTree.Bits qualified as BSF
import Data.Png qualified as Png

import Control.Monad.Yaftee.Pipe.Png.Chunk qualified as Chunk

chunks :: U.Member Pipe.P es => a -> Eff.E es (a -> (Chunk, a)) BSF.ByteString a
chunks x = Pipe.yield Png.fileHeader >> fvr x \st -> Pipe.await >>= \f -> do
	let	(c, st') = f st
	st' <$ Pipe.yield (chunkToByteString c)
	where fvr st a = a st >>= (`fvr` a)

data Chunk = Chunk { chunkName :: BSF.ByteString, chunkBody :: BSF.ByteString }
	deriving Show

chunkToByteString :: Chunk -> BSF.ByteString
chunkToByteString Chunk { chunkName = nm, chunkBody = bd } =
	BSF.fromBitsBE' ln <> nmbd <> BSF.fromBitsBE' (Crc32.toWord crc)
	where
	ln = fromIntegral @_ @Word32 $ BSF.length bd
	nmbd = nm <> bd
	crc = Crc32.complement $ BSF.foldl' Crc32.step Crc32.initial nmbd

chunkToChunk :: U.Member Pipe.P es => Eff.E es Chunk Chunk.C r
chunkToChunk = forever do
	Chunk cnm cbd <- Pipe.await
	Pipe.yield $ Chunk.Begin (olength cbd) cnm
	Pipe.yield $ Chunk.Body cbd
	Pipe.yield $ Chunk.End

chunks' :: (
	U.Member Pipe.P es, U.Member (State.Named "foo" Crc32.C) es,
	U.Member (Except.E String) es, U.Member Fail.F es
	) =>
	Eff.E es Chunk BSF.ByteString ()
chunks' = void $ chunkToChunk Pipe.=$= Chunk.encode "foo"

chunkToChunkSt st0 = fvr st0 \st -> Pipe.await >>= \f -> do
	let	(Chunk cnm cbd, st') = f st
	st' <$ do
		Pipe.yield $ Chunk.Begin (olength cbd) cnm
		Pipe.yield $ Chunk.Body cbd
		Pipe.yield $ Chunk.End
	where fvr st a = a st >>= (`fvr` a)

chunksSt :: (
	U.Member Pipe.P es, U.Member (State.Named "foo" Crc32.C) es,
	U.Member (Except.E String) es, U.Member Fail.F es ) =>
	a -> Eff.E es (a -> (Chunk, a)) BSF.ByteString ()
chunksSt st0 = void $ chunkToChunkSt st0 Pipe.=$= Chunk.encode "foo"
