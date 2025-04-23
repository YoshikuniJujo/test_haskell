{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module UseFTCQ.Eff where

import Control.Monad.Fix
import UseFTCQ.HFreer qualified as HFreer
import Data.FTCQueue qualified as Q
import OpenUnion qualified as Union

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

handleRelay' :: Union.HFunctor (Union.U effs) =>
--	(a -> b) ->
--	(a -> E effs b) ->
	(forall v b . eff v -> (v -> E effs b) -> E effs b) ->
	E ((Union.FromFirst eff) ': effs) a -> E effs a
handleRelay' h = fix \go -> \case
	HFreer.Pure x -> HFreer.Pure x
	u HFreer.:>>= q -> case Union.decomp u of
		Left u' -> Union.hmap (handleRelay' h) id u' HFreer.:>>= Q.singleton (go `HFreer.comp` q)
		Right (Union.FromFirst x) -> h x (go `HFreer.comp` q)
