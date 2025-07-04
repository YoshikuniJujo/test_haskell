{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.Buffer (

	run, getInput, Monoid(..)

	) where

import Prelude hiding (Monoid)
import Prelude qualified as P
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.State qualified as State
import Control.HigherOpenUnion qualified as U
import Data.HigherFunctor qualified as HFunctor
import Data.ByteString.FingerTree qualified as BSF

run :: forall nm m es i o r . (P.Monoid m, HFunctor.Loose (U.U es)) =>
	Eff.E (State.Named nm (Monoid m) ': es) i o r ->
	Eff.E es i o (r, Monoid m)
run = flip (State.runN @nm) (Monoid mempty)

getInput :: forall nm -> (
	U.Member Pipe.P es,
	U.Member (State.Named nm (Monoid BSF.ByteString)) es ) =>
	Int -> Eff.E es BSF.ByteString o BSF.ByteString
getInput nm n = State.getN nm >>= \(Monoid bs) -> case BSF.splitAt' n bs of
	Nothing -> readMore nm >> getInput nm n
	Just (t, d) -> t <$ State.putN nm (Monoid d)

readMore :: forall nm -> (
	Semigroup mono, U.Member Pipe.P es,
	U.Member (State.Named nm (Monoid mono)) es ) =>
	Eff.E es mono o ()
readMore nm =
	Pipe.await >>= \xs -> State.modifyN nm (Monoid . (<> xs) . unMonoid)

newtype Monoid m = Monoid { unMonoid :: m } deriving Show
