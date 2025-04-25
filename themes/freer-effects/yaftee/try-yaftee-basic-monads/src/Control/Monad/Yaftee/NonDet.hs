{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.NonDet (N, run, split) where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.HFreer qualified as HFreer
import Control.HigherOpenUnion qualified as Union
import Data.HFunctor qualified as HFunctor
import Data.List.ToolsYj
import Data.FTCQueue qualified as Q

type N = Union.FromFirst Union.NonDet

run :: (HFunctor.H (Union.U effs), Traversable f, MonadPlus f) =>
	Eff.E (N ': effs) a -> Eff.E effs (f a)
run = \case
	HFreer.Pure x -> HFreer.Pure $ pure x
	u HFreer.:>>= q -> case Union.decomp u of
		Left u' -> HFunctor.map run pure u' HFreer.:>>=
			Q.singleton \xs ->
				join <$> (run . (q `HFreer.app`)) `traverse` xs
		Right (Union.FromFirst Union.MZero _) -> pure empty
		Right (Union.FromFirst Union.MPlus k) -> (<|>)
			<$> run (q `HFreer.app` k False)
			<*> run (q `HFreer.app` k True)

split :: (Union.Member N effs) =>
	Eff.E effs a -> Eff.E effs (Maybe (a, Eff.E effs a))
split = ($ []) $ fix \go jq -> \case
	HFreer.Pure x -> pure (Just (x, msum jq))
	u HFreer.:>>= q -> case Union.prj u of
		Nothing -> u HFreer.:>>= Q.singleton (go jq `HFreer.comp` q)
		Just (Union.FromFirst Union.MZero _k) ->
			list (pure Nothing) (flip go) jq
		Just (Union.FromFirst Union.MPlus k) ->
			go (q `HFreer.app` k True : jq) (q `HFreer.app` k False)
