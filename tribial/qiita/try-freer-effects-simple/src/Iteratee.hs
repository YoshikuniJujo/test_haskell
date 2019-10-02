{-# LANGUAGE GADTs, DataKinds, TypeOperators, FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Iteratee where

import Eff

data Iteratee i a where Get :: Iteratee i i

get :: Member (Iteratee i) effs => Eff effs i
get = inj Get `Bind` Pure

runIteratee :: Eff (Iteratee i ': effs) a -> [i] -> Eff effs (Maybe a)
runIteratee m ia@(~(i : is)) = case m of
	Pure x -> Pure $ Just x
	u `Bind` k -> case decomp u of
		Right Get -> case ia of
			[] -> Pure Nothing
			_ -> runIteratee (k i) is
		Left u' -> u' `Bind` ((`runIteratee` ia) . k)
