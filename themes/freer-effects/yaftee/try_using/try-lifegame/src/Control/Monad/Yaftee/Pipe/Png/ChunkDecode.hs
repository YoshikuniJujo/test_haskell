{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.Png.ChunkDecode (

	Chunk(..), isChunkName,

	chunkRun_', ChunkStates', chunk', ChunkMembers',

	) where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.ByteString.FingerTree.OnDemand qualified as OnDemand
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.Fail qualified as Fail
import Control.HigherOpenUnion qualified as U
import Data.TypeLevel.List
import Data.HigherFunctor qualified as HFunctor
import Data.Char
import Data.Word.Crc32 qualified as Crc32
import Data.ByteString.FingerTree qualified as BSF

import Control.Monad.Yaftee.Pipe.Png.Chunk qualified as ChunkNew

chunkRun_' :: forall nm es i o r . HFunctor.Loose (U.U es) =>
	Eff.E (ChunkStates' nm `Append` es) i o r ->
	Eff.E es i o ()
chunkRun_' = void
	. OnDemand.run
	. (`State.runN` Crc32.initial)
	. (`State.runN` Chunk False (seqFromString "IHDR"))

type ChunkStates' nm =
	'[State.Named nm Chunk, State.Named nm Crc32.C] `Append`
	OnDemand.States nm

chunk' :: forall nm -> (
	U.Member Pipe.P es,
	ChunkMembers' nm es,
	U.Member (Except.E String) es, U.Member Fail.F es
	) =>
	Int -> Eff.E es BSF.ByteString BSF.ByteString ()
chunk' nm n = void $ ChunkNew.decode nm n Pipe.=$= chunkAfter nm

type ChunkMembers' nm es = (
	OnDemand.Members nm es,
	U.Member (State.Named nm Crc32.C) es,
	U.Member (State.Named nm Chunk) es )

chunkAfter :: forall nm -> (
	U.Member Pipe.P es,
	U.Member (State.Named nm Chunk) es,
	U.Member (Except.E String) es,
	U.Member Fail.F es
	) =>
	Eff.E es ChunkNew.C BSF.ByteString ()
chunkAfter nm = forever $ Pipe.await >>= \case
	ChunkNew.Begin _ cnm -> do
		State.putN nm $ Chunk True cnm
		Pipe.yield ""
		State.putN nm $ Chunk False cnm
		fix \go -> Pipe.await >>= \case
			ChunkNew.Body bd -> Pipe.yield bd >> go
			ChunkNew.End -> pure ()
			ChunkNew.EndOfTheWorld -> Pipe.yield "ENDOFTHEWORLD"
			_ -> Except.throw @String "chunkAfter: bad"
	ChunkNew.EndOfTheWorld -> Pipe.yield "ENDOFTHEWORLD"

seqFromString :: String -> BSF.ByteString
seqFromString = BSF.pack . (fromIntegral . ord <$>)

data Chunk = Chunk {
	chunkBegin :: Bool,
	chunkName :: BSF.ByteString
	}
	deriving (Show, Eq)

isChunkName :: BSF.ByteString -> Chunk -> Bool
isChunkName nm0 cn = chunkName cn == nm0
