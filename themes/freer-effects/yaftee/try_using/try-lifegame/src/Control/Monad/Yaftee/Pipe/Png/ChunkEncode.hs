{-# LANGUAGE ImportQualifiedPost, PackageImports #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.Png.ChunkEncode (
	chunksSt, Chunk(..)
	) where

import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.Fail qualified as Fail
import Control.HigherOpenUnion qualified as U
import Data.MonoTraversable
import Data.Word.Crc32 qualified as Crc32
import Data.ByteString.FingerTree qualified as BSF

import Control.Monad.Yaftee.Pipe.Png.Chunk qualified as Chunk

data Chunk = Chunk { chunkName :: BSF.ByteString, chunkBody :: BSF.ByteString }
	deriving Show

chunksSt :: (
	U.Member Pipe.P es, U.Member (State.Named "foo" Crc32.C) es,
	U.Member (Except.E String) es, U.Member Fail.F es ) =>
	a -> Eff.E es (a -> (Chunk, a)) BSF.ByteString ()
chunksSt st0 = void $ chunkToChunkSt st0 Pipe.=$= Chunk.encode "foo"

chunkToChunkSt :: U.Member Pipe.P es =>
	a -> Eff.E es (a -> (Chunk, a)) Chunk.C r
chunkToChunkSt st0 = fvr st0 \st -> Pipe.await >>= \f -> do
	let	(Chunk cnm cbd, st') = f st
	st' <$ do
		Pipe.yield $ Chunk.Begin (olength cbd) cnm
		Pipe.yield $ Chunk.Body cbd
		Pipe.yield $ Chunk.End
	where fvr st a = a st >>= (`fvr` a)
