{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.MonoTraversable (

	lengthRun, length, length', Length, lengthFromInt64, lengthToInt64

	) where

import Prelude hiding (length)
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.State qualified as State
import Control.HigherOpenUnion qualified as U
import Data.HigherFunctor qualified as HFunctor
import Data.MonoTraversable
import Data.Int

lengthRun :: forall nm es i o r . HFunctor.Loose (U.U es) =>
	Eff.E (State.Named nm Length ': es) i o r -> Eff.E es i o (r, Length)
lengthRun = (`State.runN` (0 :: Length))

length :: forall nm -> (
	MonoFoldable mono,
	U.Member Pipe.P es,
	U.Member (State.Named nm Length) es ) =>
	Eff.E es mono mono r
length nm = do
	State.putN nm $ Length 0
	forever $ Pipe.await >>= \xs ->
		State.modifyN nm (+ Length (olength64 xs)) >> Pipe.yield xs

length' :: forall nm -> (
	MonoFoldable mono,
	U.Member Pipe.P es,
	U.Member (State.Named nm Length) es ) =>
	Eff.E es mono mono ()
length' nm = do
	State.putN nm $ Length 0
	fix \go -> Pipe.awaitMaybe >>= \case
		Nothing -> pure ()
		Just xs -> (>> go) do
			State.modifyN nm (+ Length (olength64 xs))
			Pipe.yield xs

newtype Length = Length { unLength :: Int64 }
	deriving (Show, Eq, Ord, Enum, Num, Real, Integral)

lengthToInt64 :: Length -> Int64
lengthToInt64 = unLength

lengthFromInt64 :: Int64 -> Length
lengthFromInt64 = Length
