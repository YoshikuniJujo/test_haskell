{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.Png.Decode (

	-- * DECODE HEADER

	runHeader, StatesHeader, decodeHeader, MembersHeader,

	) where

import Prelude hiding (Monoid)
import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.ByteString.FingerTree.OnDemand qualified as OnDemand
import Control.Monad.Yaftee.Pipe.Png.Decode.Header qualified as Header
import Control.Monad.Yaftee.Pipe.Png.Decode.Chunk qualified as Chunk
import Control.Monad.Yaftee.Pipe.BytesCrc32 qualified as Chunk
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.HigherOpenUnion qualified as U
import Data.TypeLevel.List
import Data.HigherFunctor qualified as HFunctor
import Data.ByteString.FingerTree qualified as BSF
import Data.Png
import Data.Png.Header.Data qualified as Header

data ColorType = Rgb | Rgba deriving Show
data BitDepth = BitDepth8 | BitDepth16 deriving Show

runHeader :: forall nm es i o r . HFunctor.Loose (U.U es) =>
	Eff.E (StatesHeader nm `Append` es) i o r -> Eff.E es i o Header.Header
runHeader = (snd <$>)
	. flip (State.runN @nm) Header.header0
	. OnDemand.run @nm
	. Chunk.chunkRun_ @nm

type StatesHeader nm =
	Chunk.ChunkStates nm `Append`
	OnDemand.States nm `Append` '[State.Named nm Header.Header]

decodeHeader :: forall nm -> (
	U.Member Pipe.P es,
	MembersHeader nm es,
	U.Member (Except.E String) es ) =>
	Eff.E es BSF.ByteString o ()
decodeHeader nm = void $
	do	fhdr <- Chunk.readBytes nm 8
		when (fhdr /= fileHeader)
			$ Except.throw @String "decodeHeader: File header error"
		Chunk.chunk nm 500
	Pipe.=$= forever do
		x <- Pipe.await
		c <- State.getN nm
		when ("IHDR" `Chunk.isChunkName` c || "IDAT" `Chunk.isChunkName` c)
			$ Pipe.yield x
	Pipe.=$= OnDemand.onDemand nm Pipe.=$= Header.read nm (const $ pure ())

type MembersHeader nm es = (
	Chunk.ChunkMembers nm es, OnDemand.Members nm es,
	U.Member (State.Named nm Header.Header) es )
