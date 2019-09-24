{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module State (State, runState, get, put, modify) where

import Eff (Eff, Member, send, Freer(..), decomp)

data State s a where Get :: State s s; Put :: !s -> State s ()

get :: Member (State s) effs => Eff effs s
get = send Get

put :: Member (State s) effs => s -> Eff effs ()
put = send . Put

modify :: Member (State s) effs => (s -> s) -> Eff effs ()
modify f = put . f =<< get

runState :: Eff (State s ': effs) a -> s -> Eff effs (a, s)
m `runState` s0 = case m of
	Pure x -> Pure (x, s0)
	u `Bind` k -> case decomp u of
		Right Get -> runState (k s0) s0
		Right (Put s) -> runState (k ()) s
		Left u' -> u' `Bind` ((`runState` s0) . k)
