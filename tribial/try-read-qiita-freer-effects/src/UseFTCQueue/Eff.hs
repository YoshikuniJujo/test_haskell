{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module UseFTCQueue.Eff (
	Eff, Freer(..), Member, eff, run, runM, prj, decomp, weaken,
	handleRelay, interpose
	) where

import Control.Monad.Fix
import UseFTCQueue.Freer
import OpenUnion

type Eff effs = Freer (Union effs)

run :: Eff '[] a -> a
run (Pure x) = x
run _ = error "Eff.run: This function can run only Pure"

runM :: Monad m => Eff '[m] a -> m a
runM (Pure x) = pure x
runM (u `Bind` q) = runM . (q `app`) =<< extract u

eff :: Member t effs => t a -> Eff effs a
eff = (`Bind` singleton Pure) . inj

handleRelay ::
	(a -> Eff effs b) ->
	(forall v . eff v -> (v -> Eff effs b) -> Eff effs b) ->
	Eff (eff ': effs) a -> Eff effs b
handleRelay ret h = fix \go -> \case
	Pure x -> ret x
	u `Bind` q -> case decomp u of
		Left u -> u `Bind` singleton (go `comp` q)
		Right x -> h x (go `comp` q)

interpose :: Member eff effs =>
	(a -> Eff effs b) ->
	(forall v . eff v -> (v -> Eff effs b) -> Eff effs b) ->
	Eff effs a -> Eff effs b
interpose ret h = fix \go -> \case
	Pure x -> ret x
	u `Bind` q -> case prj u of
		Just x -> h x (go `comp` q)
		_ -> u `Bind` singleton (go `comp` q)
