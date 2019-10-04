{-# LANGUAGE ScopedTypeVariables, GADTs, DataKinds, TypeOperators, FlexibleContexts, AllowAmbiguousTypes, TypeApplications #-}
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

viewIteratee :: forall effs i a . Member (Iteratee i) effs => Eff effs a -> Eff effs (Maybe (i -> Eff effs a))
viewIteratee m = case m of
	Pure _ -> return Nothing
	u `Bind` k -> case prj u of
		Just (Get :: Iteratee i x) -> return $ Just k
		Nothing -> u `Bind` (viewIteratee . k)

par :: forall effs i a b . Member (Iteratee i) effs =>
	Eff effs a -> Eff effs b -> Eff effs (Eff effs a, Eff effs b)
par l r = do
	(vl :: Maybe (i -> Eff effs a)) <- viewIteratee l
	(vr :: Maybe (i -> Eff effs b)) <- viewIteratee r
	case (vl, vr) of
		(Nothing, _) -> return (l, r)
		(_, Nothing) -> return (l, r)
		(Just l', Just r') -> get >>= \x -> par @_ @i (l' x) (r' x)
