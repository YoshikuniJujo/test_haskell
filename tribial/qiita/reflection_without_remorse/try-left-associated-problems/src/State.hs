{-# LANGUAGE GADTs, DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module State (State, runState, get, put, modify, sampleL, sampleR) where

import Eff (Eff, Freer(..), Member, inj, decomp)

data State s a where Get :: State s s; Put :: s -> State s ()

get :: Member (State s) effs => Eff effs s
get = inj Get `Bind` Pure

put :: Member (State s) effs => s -> Eff effs ()
put = (`Bind` Pure) . inj . Put

modify :: Member (State s) effs => (s -> s) -> Eff effs ()
modify f = put . f =<< get

runState :: Eff (State s ': effs) a -> s -> Eff effs (a, s)
m `runState` s0 = case m of
	Pure x -> Pure (x, s0)
	u `Bind` k -> case decomp u of
		Right Get -> k s0 `runState` s0
		Right (Put s) -> k () `runState` s
		Left u' -> u' `Bind` ((`runState` s0) . k)

sampleL, sampleR :: Member (State Integer) effs => Eff effs ()
sampleL = {-# SCC "LeftAssociatedCounter" #-}
	foldl (>>) (pure ()) . replicate 8000 $ modify (+ (1 :: Integer))
sampleR = {-# SCC "RightAssociatedCounter" #-}
	foldr (>>) (pure ()) . replicate 8000 $ modify (+ (1 :: Integer))
