{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.Png.Decode.Steps (
	chunk
	) where

import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.BytesCrc32 qualified as Chunk
import Control.Monad.Yaftee.Pipe.Png.Decode.Chunk qualified as Chunk
import Control.Monad.Yaftee.Except qualified as Except
import Control.HigherOpenUnion qualified as U
import Data.ByteString.FingerTree qualified as BSF
import Data.Png

chunk :: forall nm -> (
	U.Member Pipe.P es,
	Chunk.ChunkMembers nm es,
	U.Member (Except.E String) es
	) =>
	Eff.E es BSF.ByteString BSF.ByteString ()
chunk nm =
	do	fhdr <- Chunk.readBytes nm 8
		when (fhdr /= fileHeader)
			. Except.throw @String
			$ "chunk: File header error: " <> show fhdr
		Chunk.chunk nm 500
