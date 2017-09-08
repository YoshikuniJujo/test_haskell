{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds, TypeOperators #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}

module MyEff.Internal (
	Eff, Member, run, runM, send, handleRelay, handleRelayS, interpose,
	Freer(..), decomp, qApp, qComp, tsingleton, prj, extract ) where

import Freer (Freer(..), tsingleton, qApp, qComp)
import OpenUnion (Union, Member, inj, prj, decomp, extract)

type Eff effs = Freer (Union effs)

run :: Eff '[] a -> a
run (Pure x) = x
run _ = error "MyEff.run: expect Pure"

runM :: Monad m => Eff '[m] a -> m a
runM (Pure x) = return x
runM (Join u q) = runM . (q `qApp`) =<< extract u

send :: Member eff effs => eff a -> Eff effs a
send = (`Join` tsingleton Pure) . inj

handleRelay ::
	(a -> Eff effs b) ->
	(forall x . eff x -> (x -> Eff effs b) -> Eff effs b) ->
	Eff (eff ': effs) a -> Eff effs b
handleRelay ret h = handleRelayS () (const ret) h'
	where h' () m k = h m (k ())

handleRelayS ::
	s -> (s -> a -> Eff effs b) ->
	(forall x . s -> eff x -> (s -> x -> Eff effs b) -> Eff effs b) ->
	Eff (eff ': effs) a -> Eff effs b
handleRelayS s0 ret h = loop s0
	where loop s = \case
		Pure x -> ret s x
		Join u q -> case decomp u of
			Right tx -> h s tx f
			Left u' -> Join u' . tsingleton $ f s
			where f = (. (q `qApp`)) . loop

interpose :: Member eff effs =>
	(a -> Eff effs b) ->
	(forall v . eff v -> (v -> Eff effs b) -> Eff effs b) ->
	Eff effs a -> Eff effs b
interpose ret h = loop
	where loop = \case
		Pure x -> ret x
		Join u q -> case prj u of
			Just x -> h x k
			Nothing -> Join u $ tsingleton k
			where k = q `qComp` loop
