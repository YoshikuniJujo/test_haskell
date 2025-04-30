{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Yaftee.UseFTCQ.Eff where

import Control.Monad.Fix

import Yaftee.UseFTCQ.HFreer qualified as HFreer
import Yaftee.OpenUnion qualified as Union
import Yaftee.HFunctor qualified as Union

import Data.FTCQueue qualified as Q

type E effs = HFreer.H (Union.U effs)

eff :: Union.Member (Union.FromFirst t) effs => t a -> E effs i o a
eff = (HFreer.:>>= Q.singleton HFreer.Pure) . Union.inj

effBase :: Union.Base (Union.FromFirst t) effs => t a -> E effs i o a
effBase = (HFreer.:>>= Q.singleton HFreer.Pure) . Union.injBase

effh :: Union.Member h effs => h (E effs) i o a -> E effs i o a
effh = (HFreer.:>>= Q.singleton HFreer.Pure) . Union.injh

run :: E '[] i o a -> a
run (HFreer.Pure x) = x
run _ = error "bad"

runM :: Monad m => E '[Union.FromFirst m] i o a -> m a
runM (HFreer.Pure x) = pure x
runM (u HFreer.:>>= q) = runM . (q `HFreer.app`) =<< Union.extract u

handleRelay :: Union.HFunctor (Union.U effs) =>
	(forall x . x -> t x) -> (forall x . t x -> x) ->
	(forall v b i o . eff v -> (v -> E effs i o (t b)) -> E effs i o (t b)) ->
	E ((Union.FromFirst eff) ': effs) i o a -> E effs i o (t a)
handleRelay mk gx h = fix \go -> \case
	HFreer.Pure x -> HFreer.Pure $ mk x
	u HFreer.:>>= q -> case Union.decomp u of
		Left u' -> Union.hmap (handleRelay mk gx h) mk u' HFreer.:>>=
			Q.singleton \x -> go $ q `HFreer.app` gx x
		Right (Union.FromFirst x k) -> h x (\x' -> (go `HFreer.comp` q) $ k x')

handleRelayS :: Union.HFunctor (Union.U effs) =>
	(forall x . x -> s -> t s x) ->
	(forall x . t s x -> x) -> (forall x . t s x -> s) ->
	(forall v b i o . eff v -> (v -> s -> E effs i o b) -> s -> E effs i o b) ->
	E ((Union.FromFirst eff) ': effs) i o a -> s -> E effs i o (t s a)
handleRelayS mk gx gs h m s0 = ($ m) . ($ s0) $ fix \go s -> \case
	HFreer.Pure x -> HFreer.Pure $ mk x s
	u HFreer.:>>= q -> case Union.decomp u of
		Left u' -> Union.hmap (flip (handleRelayS mk gx gs h) s) (`mk` s) u'
			HFreer.:>>= Q.singleton \xs -> go (gs xs) $ q `HFreer.app` gx xs
		Right (Union.FromFirst x k) -> h x (\x' s' -> (go  s' `HFreer.comp` q) . k $ x') s
