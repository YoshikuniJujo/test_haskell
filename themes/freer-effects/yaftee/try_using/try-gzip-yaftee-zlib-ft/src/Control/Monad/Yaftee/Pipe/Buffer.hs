{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.Buffer (

	run, format, getInput, Monoid(..)

	) where

import Prelude hiding (Monoid)
import Prelude qualified as P
import Control.Monad.Fix
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.State qualified as State
import Control.HigherOpenUnion qualified as U
import Data.HigherFunctor qualified as HFunctor

run :: forall nm m es i o r . (P.Monoid m, HFunctor.Loose (U.U es)) =>
	Eff.E (State.Named nm (Monoid m) ': es) i o r ->
	Eff.E es i o (r, Monoid m)
run = flip (State.runN @nm) (Monoid mempty)

format :: forall nm -> (
	Semigroup m,
	U.Member Pipe.P es, U.Member (State.Named nm (Monoid m)) es ) =>
	(Int -> m -> Maybe (m, m)) ->
	m -> [Int] -> Eff.E es m m ()
format nm sp bs0 ns0 = do
	State.putN nm $ Monoid bs0
	($ ns0) $ fix \go -> \case
		[] -> pure ()
		n : ns -> do
			Pipe.yield =<< getInput nm sp n
			go ns

getInput :: forall nm -> (
	Semigroup m,
	U.Member Pipe.P es, U.Member (State.Named nm (Monoid m)) es ) =>
	(Int -> m -> Maybe (m, m)) -> Int -> Eff.E es m o m
getInput nm sp n = State.getN nm >>= \(Monoid bs) -> case sp n bs of
	Nothing -> readMore nm >> getInput nm sp n
	Just (t, d) -> t <$ State.putN nm (Monoid d)

readMore :: forall nm -> (
	Semigroup mono, U.Member Pipe.P es,
	U.Member (State.Named nm (Monoid mono)) es ) =>
	Eff.E es mono o ()
readMore nm =
	Pipe.await >>= \xs -> State.modifyN nm (Monoid . (<> xs) . unMonoid)

newtype Monoid m = Monoid { unMonoid :: m } deriving Show
