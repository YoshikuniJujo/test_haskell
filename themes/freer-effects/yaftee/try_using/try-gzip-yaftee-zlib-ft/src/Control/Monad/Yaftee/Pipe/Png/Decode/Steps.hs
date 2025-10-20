{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.Png.Decode.Steps (
	module Control.Monad.Yaftee.Pipe.Png.Decode.Steps,
	Chunk.Chunk(..)
	) where

import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.BytesCrc32 qualified as Chunk
import Control.Monad.Yaftee.Pipe.Png.Decode.Chunk qualified as Chunk
import Control.Monad.Yaftee.Except qualified as Except
import Control.HigherOpenUnion qualified as U
import Data.TypeLevel.List
import Data.HigherFunctor qualified as HFunctor
import Data.ByteString.FingerTree qualified as BSF
import Data.Png

chunkRun_ :: forall nm es i o r . HFunctor.Loose (U.U es) =>
	Eff.E (ChunkStates nm `Append` es) i o r -> Eff.E es i o ()
chunkRun_ = void . Chunk.chunkRun_ @nm

type ChunkStates nm =
	Chunk.ChunkStates nm

chunk :: forall nm -> (
	U.Member Pipe.P es,
	ChunkMembers nm es,
	U.Member (Except.E String) es
	) =>
	Eff.E es BSF.ByteString BSF.ByteString ()
chunk nm =
	do	fhdr <- Chunk.readBytes nm 8
		when (fhdr /= fileHeader)
			$ Except.throw @String "File header error"
		Chunk.chunk nm 500

type ChunkMembers nm es = (
	Chunk.ChunkMembers nm es )
