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
	(\s -> pure . (, s))
	(\s m k -> case m of
		Get -> k s s
		Put s' -> k s' ())
	m0
