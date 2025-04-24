{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Eff (
	E, eff, effh, run, runM, handleRelay, handleRelayS
	) where

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

handleRelay :: HFunctor.H (Union.U effs) =>
	(forall x . x -> t x) -> (forall x . t x -> x) ->
	(forall v b . eff v -> (v -> E effs b) -> E effs b) ->
	E ((Union.FromFirst eff) ': effs) a -> E effs (t a)
handleRelay mk gx h = fix \go -> \case
	HFreer.Pure x -> HFreer.Pure $ mk x
	u HFreer.:>>= q -> case Union.decomp u of
		Left u' -> HFunctor.map (handleRelay mk gx h) mk u' HFreer.:>>=
			Q.singleton \x -> go $ q `HFreer.app` gx x
		Right (Union.FromFirst x k) -> h x (\x' -> (go `HFreer.comp` q) $ k x')

handleRelayS :: HFunctor.H (Union.U effs) =>
	(forall x . x -> s -> t x s) ->
	(forall x . t x s -> x) -> (forall x . t x s -> s) ->
	(forall v b . eff v -> (v -> s -> E effs b) -> s -> E effs b) ->
	E ((Union.FromFirst eff) ': effs) a -> s -> E effs (t a s)
handleRelayS mk gx gs h m s0 = ($ m) . ($ s0) $ fix \go s -> \case
	HFreer.Pure x -> HFreer.Pure $ mk x s
	u HFreer.:>>= q -> case Union.decomp u of
		Left u' -> HFunctor.map (flip (handleRelayS mk gx gs h) s) (`mk` s) u'
			HFreer.:>>= Q.singleton \xs -> go (gs xs) $ q `HFreer.app` (gx xs)
		Right (Union.FromFirst x k) -> h x (\x' s' -> (go s' `HFreer.comp` q) . k $ x') s
