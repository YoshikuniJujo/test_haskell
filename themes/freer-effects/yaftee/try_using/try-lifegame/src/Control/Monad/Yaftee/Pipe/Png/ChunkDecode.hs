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

	-- * RUN AND DECODE

	run_, States, decode, Members,

	-- * CHUNK TYPE

	Chunk(..), isNameOf,

	) where

import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.ByteString.FingerTree.OnDemand qualified as OnDemand
import Control.Monad.Yaftee.Pipe.Png.Chunk qualified as Chunk
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.HigherOpenUnion qualified as U
import Data.TypeLevel.List
import Data.HigherFunctor qualified as HFunctor
import Data.Function
import Data.Word.Crc32 qualified as Crc32
import Data.ByteString.FingerTree qualified as BSF

-- RUN AND DECODE

run_ :: forall nm es i o r . HFunctor.Loose (U.U es) =>
	Eff.E (States nm `Append` es) i o r -> Eff.E es i o ()
run_ = void
	. OnDemand.run . Chunk.decodeRun_ . (`State.runN` Chunk False ("IHDR"))

type States nm =
	'[State.Named nm Chunk, State.Named nm Crc32.C] `Append`
	OnDemand.States nm

decode :: forall nm -> (
	Members nm es, U.Member Pipe.P es,
	U.Member (Except.E String) es ) =>
	Int -> Eff.E es BSF.ByteString BSF.ByteString ()
decode nm n = void $ Chunk.decode nm n Pipe.=$= after nm

type Members nm es = (
	OnDemand.Members nm es,
	U.Member (State.Named nm Crc32.C) es,
	U.Member (State.Named nm Chunk) es )

after :: forall nm -> (
	Members nm es, U.Member Pipe.P es,
	U.Member (Except.E String) es ) => Eff.E es Chunk.C BSF.ByteString ()
after nm = forever $ Pipe.await >>= \case
	Chunk.Begin _ cnm -> do
		State.putN nm $ Chunk True cnm
		Pipe.yield ""
		State.putN nm $ Chunk False cnm
		fix \go -> Pipe.await >>= \case
			Chunk.Body bd -> Pipe.yield bd >> go
			Chunk.End -> pure ()
			Chunk.EndOfTheWorld -> Pipe.yield "ENDOFTHEWORLD"
			_ -> Except.throw @String "after: bad"
	Chunk.EndOfTheWorld -> Pipe.yield "ENDOFTHEWORLD"
	_ -> error "after: bad"

-- CHUNK TYPE

data Chunk = Chunk { begin :: Bool, name :: BSF.ByteString } deriving (Show, Eq)

isNameOf :: BSF.ByteString -> Chunk -> Bool
isNameOf nm0 = (== nm0) . name
