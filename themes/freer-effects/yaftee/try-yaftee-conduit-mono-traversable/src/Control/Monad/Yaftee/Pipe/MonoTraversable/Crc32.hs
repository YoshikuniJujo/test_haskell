{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.MonoTraversable.Crc32 (

	run, complement, crc32, crc32',

	step

	) where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.State qualified as State
import Control.HigherOpenUnion qualified as U
import Data.HigherFunctor qualified as HFunctor
import Data.MonoTraversable
import Data.Word
import Data.Word.Crc32 qualified as Crc32

run :: forall nm es i o r . HFunctor.Loose (U.U es) =>
	Eff.E (State.Named nm Crc32.C ': es) i o r -> Eff.E es i o (r, Crc32.C)
run = (`State.runN` Crc32.initial)

complement ::
	forall nm -> U.Member (State.Named nm Crc32.C) es => Eff.E es i o ()
complement nm = State.modifyN nm Crc32.complement

crc32 :: forall nm -> (
	MonoFoldable mono, Element mono ~ Word8,
	U.Member Pipe.P es, U.Member (State.Named nm Crc32.C) es ) =>
	Eff.E es mono mono r
crc32 nm = do
	State.putN nm Crc32.initial
	forever $ Pipe.await >>= \s ->
		State.modifyN nm (`step` s) >> Pipe.yield s

crc32' :: forall nm -> (
	MonoFoldable mono, Element mono ~ Word8,
	U.Member Pipe.P es, U.Member (State.Named nm Crc32.C) es ) =>
	Eff.E es mono mono ()
crc32' nm = do
	State.putN nm Crc32.initial
	fix \go -> Pipe.awaitMaybe >>= \case
		Nothing -> pure ()
		Just bs -> (>> go)
			$ State.modifyN nm (`step` bs) >> Pipe.yield bs

step :: (MonoFoldable mono, Element mono ~ Word8) => Crc32.C -> mono -> Crc32.C
step = ofoldl' Crc32.step
