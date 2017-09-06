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
runState m s = case m of
	Pure x -> Pure (x, s)
	Join u q -> case decomp u of
		Right Get -> runState (q `qApp` s) s
		Right (Put s') -> runState (q `qApp` ()) s'
		Left u' -> Join u' . tsingleton $ q `qComp` (`runState` s)
