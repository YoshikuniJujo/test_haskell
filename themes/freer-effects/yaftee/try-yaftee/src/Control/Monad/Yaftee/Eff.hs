{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LAnGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Eff (

	-- * TYPE

	E,

	-- * INJECTION

	eff, effBase, effh,

	-- * PROJECTION

	run, runM, 

	-- * HANDLE RELAY

	handleRelay, handleRelayS, interpose

	) where

import Control.Monad.Fix
import Control.Monad.HigherFreer qualified as HFreer
import Control.HigherOpenUnion qualified as Union
import Data.HigherFunctor qualified as HFunctor
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

handleRelay :: HFunctor.Loose (Union.U effs) =>
	(forall x . x -> f x) -> (forall x . f x -> x) ->
	(forall x i o y . t x -> (x -> E effs i o (f y)) -> E effs i o (f y)) ->
	E ((Union.FromFirst t) ': effs) i' o' a -> E effs i' o' (f a)
handleRelay mk gx h = fix \go -> \case
	HFreer.Pure x -> HFreer.Pure $ mk x
	u HFreer.:>>= q -> case Union.decomp u of
		Left u' -> HFunctor.map (handleRelay mk gx h) mk u' HFreer.:>>=
			Q.singleton \x -> go $ q `HFreer.app` gx x
		Right (Union.FromFirst x k) -> h x ((go `HFreer.comp` q) . k)

handleRelayS :: HFunctor.Loose (Union.U effs) =>
	(forall x . x -> s -> f s x) ->
	(forall x . f s x -> x) -> (forall x . f s x -> s) ->
	(forall x i o y .
		t x -> (x -> s -> E effs i o y) -> s -> E effs i o y) ->
	E ((Union.FromFirst t) ': effs) i' o' a -> s -> E effs i' o' (f s a)
handleRelayS mk gx gs h m s0 = ($ m) . ($ s0) $ fix \go s -> \case
	HFreer.Pure x -> HFreer.Pure $ mk x s
	u HFreer.:>>= q -> case Union.decomp u of
		Left u' -> HFunctor.map
				(flip (handleRelayS mk gx gs h) s) (`mk` s) u'
			HFreer.:>>= Q.singleton \xs ->
				go (gs xs) $ q `HFreer.app` gx xs
		Right (Union.FromFirst x k) ->
			h x (\x' s' -> (go s' `HFreer.comp` q) . k $ x') s

interpose :: Union.Member (Union.FromFirst eff) effs =>
	(a -> E effs i o b) ->
	(forall v . eff v -> (v -> E effs i o b) -> E effs i o b) -> E effs i o a -> E effs i o b
interpose ret h = fix \go -> \case
	HFreer.Pure x -> ret x
	u HFreer.:>>= q -> case Union.prj u of
		Just (Union.FromFirst x k) -> h x ((go `HFreer.comp` q) . k)
		_ -> u HFreer.:>>= Q.singleton (go `HFreer.comp` q)
