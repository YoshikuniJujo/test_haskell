{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yafee.Eff (
	E, eff, effBase, run, runM, handleRelay, handleRelayS, interpose
	) where

import Control.Monad.Fix
import Control.Monad.Freer qualified as Freer
import Control.OpenUnion qualified as Union
import Data.FTCQueue qualified as FTCQueue

type E effs = Freer.F (Union.U effs)

run :: E '[] a -> a
run (Freer.Pure x) = x
run _ = error "Eff.run: This function can run only Pure"

runM :: Monad m => E '[m] a -> m a
runM (Freer.Pure x) = pure x
runM (u Freer.:>>= q) = runM . (q `Freer.app`) =<< Union.extract u

eff :: Union.Member t effs => t a -> E effs a
eff = (Freer.:>>= FTCQueue.singleton Freer.Pure) . Union.inj

effBase :: Union.Base t effs => t a -> E effs a
effBase = (Freer.:>>= FTCQueue.singleton Freer.Pure) . Union.injBase

handleRelay ::
	(a -> E effs b) ->
	(forall v . eff v -> (v -> E effs b) -> E effs b) ->
	E (eff ': effs) a -> E effs b
handleRelay ret h = fix \go -> \case
	Freer.Pure x -> ret x
	u Freer.:>>= q -> case Union.decomp u of
		Left u' -> u' Freer.:>>= FTCQueue.singleton (go `Freer.comp` q)
		Right x -> h x (go `Freer.comp` q)

handleRelayS ::
	(a -> s -> E effs b) ->
	(forall v . eff v -> (v -> s -> E effs b) -> s -> E effs b) ->
	E (eff ': effs) a -> s -> E effs b
handleRelayS ret h = flip $ fix \go s -> \case
	Freer.Pure x -> ret x s
	u Freer.:>>= q -> case Union.decomp u of
		Left u' -> u' Freer.:>>= FTCQueue.singleton (go s `Freer.comp` q)
		Right x -> h x (flip \s' -> go s' `Freer.comp` q) s

interpose :: Union.Member eff effs =>
	(a -> E effs b) ->
	(forall v . eff v -> (v -> E effs b) -> E effs b) ->
	E effs a -> E effs b
interpose ret h = fix \go -> \case
	Freer.Pure x -> ret x
	u Freer.:>>= q -> case Union.prj u of
		Just x -> h x (go `Freer.comp` q)
		_ -> u Freer.:>>= FTCQueue.singleton (go `Freer.comp` q)
