{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.NonDet where

import Control.Applicative
import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.HFreer qualified as HFreer
import Control.HigherOpenUnion qualified as Union
import Data.HFunctor qualified as HFunctor
import Data.FTCQueue qualified as Q

type N = Union.FromFirst Union.NonDet

run :: (HFunctor.H (Union.U effs), MonadPlus f, Traversable f) =>
	Eff.E (N ': effs) a -> Eff.E effs (f a)
-- run :: HFunctor.H (Union.U effs) => Eff.E (N ': effs) a -> Eff.E effs [a]
run = \case
	HFreer.Pure x -> HFreer.Pure $ pure x
	u HFreer.:>>= q -> case Union.decomp u of
		Left u' -> HFunctor.map run pure u' HFreer.:>>=
			Q.singleton \xs -> join <$> (run . (q `HFreer.app`)) `mapM` xs
		Right (Union.FromFirst Union.MZero k) -> pure empty
		Right (Union.FromFirst Union.MPlus k) ->
			(<|>) <$> run (q `HFreer.app` k False) <*> run (q `HFreer.app` k True)

split :: (
	Union.Member N effs
	) =>
	Eff.E effs a -> Eff.E effs (Maybe (a, Eff.E effs a))
split = go []
	where
	go jq (HFreer.Pure x) = pure (Just (x, msum jq))
	go jq (u HFreer.:>>= q) = case Union.prj u of
		Just (Union.FromFirst Union.MZero _k) -> case jq of
			[] -> pure Nothing
			j : jq' -> go jq' j
		Just (Union.FromFirst Union.MPlus k) -> go (q `HFreer.app` k False : jq) (q `HFreer.app` k True)
		Nothing -> u HFreer.:>>= Q.singleton (go jq `HFreer.comp` q)

moveKnight :: (
	Union.Member N effs
	) =>
	(Int, Int) -> Eff.E effs (Int, Int)
moveKnight (c, r) = do
	(c', r') <- asum $ pure <$> [
		(c + 2, r - 1), (c + 2, r + 1), (c - 2, r - 1), (c - 2, r + 1),
		(c + 1, r - 2), (c + 1, r + 2), (c - 1, r - 2), (c - 1, r + 2) ]
	guard (c' `elem` [1 .. 8] && r' `elem` [1 .. 8])
	pure (c', r')

in3 st = do
	ft <- moveKnight st
	sc <- moveKnight ft
	moveKnight sc
