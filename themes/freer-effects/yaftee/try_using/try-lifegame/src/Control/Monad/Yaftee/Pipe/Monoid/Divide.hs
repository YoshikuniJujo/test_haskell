{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.Monoid.Divide (

	run,
	devideNs,
	devide,
	Monoid(..)

	) where

import Prelude hiding (Monoid)
import Prelude qualified as P
import Control.Monad.Fix
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.State qualified as State
import Control.HigherOpenUnion qualified as U
import Data.HigherFunctor qualified as HFunctor
import Data.Bool

run :: forall nm m es i o r . (P.Monoid m, HFunctor.Loose (U.U es)) =>
	Eff.E (State.Named nm (Monoid m) ': es) i o r ->
	Eff.E es i o (r, Monoid m)
run = flip (State.runN @nm) (Monoid mempty)

devideNs :: forall nm -> (
	Semigroup m,
	U.Member Pipe.P es, U.Member (State.Named nm (Monoid m)) es ) =>
	(Int -> m -> Maybe (m, m)) -> m -> [Int] -> Eff.E es m m ()
devideNs nm sp bs0 ns0 = do
	State.putN nm $ Monoid bs0
	($ ns0) $ fix \go -> \case
		[] -> pure ()
		n : ns -> do
			Pipe.yield =<< getInput nm sp n
			go ns

devide :: forall nm -> (
	Eq m, P.Monoid m,
	U.Member Pipe.P es, U.Member (State.Named nm (Monoid m)) es ) =>
	(Int -> m -> Maybe (m, m)) -> m -> Int -> Eff.E es m m ()
devide nm sp bs0 n = do
	State.putN nm $ Monoid bs0
	fix \go -> do
		bs <- getInput' nm sp n
		if bs == mempty then pure () else Pipe.yield bs >> go

getInput' :: forall m es o . forall nm -> (
	P.Monoid m,
	U.Member Pipe.P es, U.Member (State.Named nm (Monoid m)) es ) =>
	(Int -> m -> Maybe (m, m)) -> Int -> Eff.E es m o m
getInput' nm sp n = State.getN nm >>= \(Monoid bs) -> case sp n bs of
	Nothing -> readMore' nm >>= bool
		(unMonoid <$> (State.getN nm <* State.putN @(Monoid m) nm (Monoid mempty)))
		(getInput' nm sp n)
	Just (t, d) -> t <$ State.putN nm (Monoid d)

readMore' :: forall nm -> (
	Semigroup mono, U.Member Pipe.P es,
	U.Member (State.Named nm (Monoid mono)) es ) =>
	Eff.E es mono o Bool
readMore' nm = Pipe.awaitMaybe >>= \case
	Nothing -> pure False
	Just xs -> True <$ State.modifyN nm (Monoid . (<> xs) . unMonoid)

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
