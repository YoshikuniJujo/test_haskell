{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds, TypeOperators #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}

module MyEff (
	Eff, Member, run, runM, send, handleRelay, handleRelayS ) where

import Freer (Freer(..), tsingleton, qApp)
import OpenUnion (Union, Member, inj, decomp, extract)

type Eff effs = Freer (Union effs)

send :: Member eff effs => eff a -> Eff effs a
send t = Join (inj t) (tsingleton Pure)

run :: Eff '[] a -> a
run (Pure x) = x
run _ = error "bad"

runM :: Monad m => Eff '[m] a -> m a
runM (Pure x) = return x
runM (Join u q) = case extract u of
	mb -> mb >>= runM . (q `qApp`)

type Arr effs a b = a -> Eff effs b

handleRelay ::
	Arr effs a b ->
	(forall v . eff v -> Arr effs v b -> Eff effs b) ->
	Eff (eff ': effs) a -> Eff effs b
handleRelay ret h = handleRelayS () (const ret) h'
	where h' () m k = h m (k ())

handleRelayS ::
	s -> (s -> Arr effs a b) ->
	(forall v . s -> eff v -> (s ->  Arr effs v b) -> Eff effs b) ->
	Eff (eff ': effs) a -> Eff effs b
handleRelayS s' ret h = loop s'
	where
	loop s (Pure x) = ret s x
	loop s (Join u' q) = case decomp u' of
		Right x -> h s x k
		Left u -> Join u (tsingleton (k s))
		where
		k s'' x = loop s'' $ q `qApp` x
