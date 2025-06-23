{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.ByteString.FingerTree.Crc32 (

	run, reset, complement, crc32, crc32',

	step

	) where

import Control.Monad.Fix
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.State qualified as State
import Control.HigherOpenUnion qualified as U
import Data.HigherFunctor qualified as HFunctor
import Data.Word.Crc32 qualified as Crc32
import Data.ByteString.FingerTree qualified as BSF

run :: forall nm es i o r . HFunctor.Loose (U.U es) =>
	Eff.E (State.Named nm Crc32.C ': es) i o r -> Eff.E es i o (r, Crc32.C)
run = (`State.runN` Crc32.initial)

reset :: forall nm -> U.Member (State.Named nm Crc32.C) es => Eff.E es i o ()
reset nm = State.putN nm Crc32.initial

complement ::
	forall nm -> U.Member (State.Named nm Crc32.C) es => Eff.E es i o ()
complement nm = State.modifyN nm Crc32.complement

crc32 :: forall nm ->
	(U.Member Pipe.P es, U.Member (State.Named nm Crc32.C) es) =>
	Eff.E es BSF.ByteString BSF.ByteString r
crc32 nm = State.putN nm Crc32.initial >> body nm

body :: forall nm ->
	(U.Member Pipe.P es, U.Member (State.Named nm Crc32.C) es) =>
	Eff.E es BSF.ByteString BSF.ByteString r
body nm = fix \go -> Pipe.await >>= \s -> do
	State.modifyN nm (`step` s)
	Pipe.yield s
	go

crc32' :: forall nm ->
	(U.Member Pipe.P es, U.Member (State.Named nm Crc32.C) es) =>
	Eff.E es BSF.ByteString BSF.ByteString ()
crc32' nm = State.putN nm Crc32.initial >> body' nm

body' :: forall nm ->
	(U.Member Pipe.P es, U.Member (State.Named nm Crc32.C) es) =>
	Eff.E es BSF.ByteString BSF.ByteString ()
body' nm = fix \go -> Pipe.awaitMaybe >>= \case
	Nothing -> pure ()
	Just s -> do
		State.modifyN nm (`step` s)
		Pipe.yield s
		go

step :: Crc32.C -> BSF.ByteString -> Crc32.C
step = BSF.foldl' Crc32.step
