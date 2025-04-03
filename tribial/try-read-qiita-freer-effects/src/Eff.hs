{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Eff (
	Eff, Freer(..), Member, run, runM, inj, prj, decomp, weaken,
	handleRelay, NonDet(..) ) where

import Freer (Freer(..))
import OpenUnion (Union, Member, inj, prj, decomp, extract, weaken, NonDet(..))

type Eff effs = Freer (Union effs)

run :: Eff '[] a -> a
run (Pure x) = x
run _ = error "Eff.run: This function can run only Pure"

runM :: Monad m => Eff '[m] a -> m a
runM (Pure x) = pure x
runM (u `Bind` q) = runM . q =<< extract u

handleRelay :: (a -> Eff effs b) ->
	(forall v . eff v -> (v -> Eff effs b) -> Eff effs b) ->
	Eff (eff ': effs) a -> Eff effs b
handleRelay ret h = go
	where
	go (Pure x) = ret x
	go (u `Bind` q) = case decomp u of
		Right x -> h x (go . q)
		Left u' -> u' `Bind` (go . q)
