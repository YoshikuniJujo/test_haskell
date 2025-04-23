{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Eff (
	E, eff, effh, run, runM, handleRelaySimple ) where

import Control.Monad.Fix
import Control.Monad.HFreer qualified as HFreer
import Control.HigherOpenUnion qualified as Union
import Data.HFunctor qualified as HFunctor
import Data.FTCQueue qualified as Q

type E effs = HFreer.H (Union.U effs)

eff :: Union.Member (Union.FromFirst t) effs => t a -> E effs a
eff = (HFreer.:>>= Q.singleton HFreer.Pure) . Union.inj

effh :: Union.Member h effs => h (E effs) a -> E effs a
effh = (HFreer.:>>= Q.singleton HFreer.Pure) . Union.injh

run :: E '[] a -> a
run (HFreer.Pure x) = x
run _ = error "Eff.run: This function can run only Pure"

runM :: Monad m => E '[(Union.FromFirst m)] a -> m a
runM (HFreer.Pure x) = pure x
runM (u HFreer.:>>= q) = runM . (q `HFreer.app`) =<< Union.extract u

handleRelaySimple :: HFunctor.H (Union.U effs) =>
	(forall v b . eff v -> (v -> E effs b) -> E effs b) ->
	E ((Union.FromFirst eff) ': effs) a -> E effs a
handleRelaySimple h = fix \go -> \case
	HFreer.Pure x -> HFreer.Pure x
	u HFreer.:>>= q -> case Union.decomp u of
		Left u' -> HFunctor.map (handleRelaySimple h) id u'
			HFreer.:>>= Q.singleton (go `HFreer.comp` q)
		Right (Union.FromFirst x) -> h x (go `HFreer.comp` q)
