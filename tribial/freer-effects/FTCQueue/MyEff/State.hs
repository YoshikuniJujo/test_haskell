{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MyEff.State where

import MyEff (Eff, Member, send, handleRelayS)

data State s a where Get :: State s s; Put :: !s -> State s ()

get :: Member (State s) effs => Eff effs s
get = send Get

put :: Member (State s) effs => s -> Eff effs ()
put = send . Put

modify :: Member (State s) effs => (s -> s) -> Eff effs ()
modify f = put . f =<< get

runState :: Eff (State s ': effs) a -> s -> Eff effs (a, s)
runState m s0 = handleRelayS s0
	((pure .) . flip (,))
	(\s tx f -> case tx of Get -> f s s; Put s' -> f s' ())
	m
