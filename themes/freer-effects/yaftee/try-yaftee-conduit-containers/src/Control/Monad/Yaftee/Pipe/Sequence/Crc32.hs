{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.Sequence.Crc32 (

	run, reset, complement, crc32,

	step

	) where

import Control.Monad.Fix
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.State qualified as State
import Control.HigherOpenUnion qualified as U
import Data.HigherFunctor qualified as HFunctor
import Data.Word
import Data.Word.Crc32 qualified as Crc32

run :: forall nm es i o r . HFunctor.Loose (U.U es) =>
	Eff.E (State.Named nm Crc32.C ': es) i o r -> Eff.E es i o (r, Crc32.C)
run = (`State.runN` Crc32.initial)

reset :: forall nm -> U.Member (State.Named nm Crc32.C) es => Eff.E es i o ()
reset nm = State.putN nm Crc32.initial

complement ::
	forall nm -> U.Member (State.Named nm Crc32.C) es => Eff.E es i o ()
complement nm = State.modifyN nm Crc32.complement

crc32 :: forall nm ->
	(Foldable t, U.Member Pipe.P es, U.Member (State.Named nm Crc32.C) es) =>
	Eff.E es (t Word8) (t Word8) r
crc32 nm = State.putN nm Crc32.initial >> body nm

body :: forall nm ->
	(Foldable t, U.Member Pipe.P es, U.Member (State.Named nm Crc32.C) es) =>
	Eff.E es (t Word8) (t Word8) r
body nm = fix \go -> Pipe.await >>= \s -> do
	State.modifyN nm (`step` s)
	Pipe.yield s
	go

step :: Foldable t => Crc32.C -> t Word8 -> Crc32.C
step = foldl Crc32.step
