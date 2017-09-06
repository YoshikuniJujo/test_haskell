{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}
{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}

module MyEff.State where

import MyEff

data State s a where
	Get :: State s s
	Put :: !s -> State s ()

get :: Member (State s) effs => Eff effs s
get = send Get

put :: Member (State s) effs => s -> Eff effs ()
put s = send (Put s)

modify :: Member (State s) effs => (s -> s) -> Eff effs ()
modify f = fmap f get >>= put

runState :: Eff (State s ': effs) a -> s -> Eff effs (a, s)
runState m0 s0 = handleRelayS s0
	(\s -> Pure . (, s))
	(\s m k -> case m of
		Get -> k s s
		Put s' -> k s' ())
	m0

handleRelayS ::
	s -> (s -> a -> Eff effs b) ->
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

type Arr effs a b = a -> Eff effs b
