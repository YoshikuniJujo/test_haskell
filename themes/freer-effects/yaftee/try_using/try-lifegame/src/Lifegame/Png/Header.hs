{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lifegame.Png.Header (

	-- * RUN

	run, States,

	-- * DECODE

	decode, Members

	) where

import Prelude hiding (Monoid, read)
import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.ByteString.FingerTree.OnDemand qualified as OnDemand
import Control.Monad.Yaftee.Pipe.Png.Chunk qualified as Chunk
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.Fail qualified as Fail
import Control.HigherOpenUnion qualified as U
import Data.TypeLevel.List
import Data.HigherFunctor qualified as HFunctor
import Data.ByteString.FingerTree qualified as BSF
import Data.Word.Word8 qualified as BSF
import Data.Png.Header.Data qualified as Header

run :: forall nm nm' es i o r . HFunctor.Loose (U.U es) =>
	Eff.E (States nm nm' `Append` es) i o r -> Eff.E es i o Header.H
run = (snd <$>) . flip (State.runN @nm') Header.header0
	. OnDemand.run @nm' . Chunk.decodeRun_ @nm

type States nm nm' =
	Chunk.DecodeStates nm `Append`
	OnDemand.States nm' `Append` '[State.Named nm' Header.H]

decode :: forall nm nm' -> (
	U.Member Pipe.P es, Members nm nm' es,
	U.Member (Except.E String) es, U.Member Fail.F es ) =>
	Eff.E es BSF.ByteString o ()
decode nm nm' = void $
	Chunk.decode nm 500
	Pipe.=$= do
		Chunk.Begin _ "IHDR" <- Pipe.await
		Chunk.Body x <- Pipe.await; Chunk.End <- Pipe.await
		Pipe.yield x
	Pipe.=$= OnDemand.onDemand nm' Pipe.=$= read nm'

type Members nm nm' es = (
	OnDemand.Members nm' es, Chunk.DecodeMembers nm es,
	U.Member (State.Named nm' Header.H) es )

read :: forall nm -> (
	U.Member Pipe.P es, OnDemand.Members nm es,
	U.Member (State.Named nm Header.H) es ) => Eff.E es BSF.ByteString o ()
read nm = do
	State.putN nm $ OnDemand.RequestBytes 4
	w <- BSF.toBitsBE <$> Pipe.await
	h <- BSF.toBitsBE <$> Pipe.await
	State.putN nm $ OnDemand.RequestBytes 1
	bd <- BSF.toBitsBE <$> Pipe.await
	ct <- BSF.toBitsBE <$> Pipe.await
	cm <- BSF.toBitsBE <$> Pipe.await
	fm <- BSF.toBitsBE <$> Pipe.await
	im <- BSF.toBitsBE <$> Pipe.await
	State.putN nm Header.H {
		Header.headerWidth = w, Header.headerHeight = h,
		Header.headerBitDepth = bd, Header.headerColorType = ct,
		Header.headerCompressionMethod = cm,
		Header.headerFilterMethod = fm,
		Header.headerInterlaceMethod = im }
