{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE GADTs, DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts, AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module IterateeEffect where

import Eff

data Iteratee i a where Get :: Iteratee i i

get :: Member (Iteratee i) effs => Eff effs i
get = inj Get `Bind` Pure

apply1 :: forall i effs a . Member (Iteratee i) effs => Eff effs a -> Eff effs (Maybe (i -> Eff effs a))
apply1 (Pure _) = pure Nothing
apply1 (u `Bind` k) = case prj @(Iteratee i) u of
	Just Get -> pure $ Just k
	Nothing -> u `Bind` \x -> apply1 (k x)

par :: forall i effs a b . Member (Iteratee i) effs =>
	Eff effs a -> Eff effs b -> Eff effs (Eff effs a, Eff effs b)
par l r	= do
	ml <- apply1 @i l
	mr <- apply1 @i r
	case (ml, mr) of
		(Nothing, _) -> pure (l, r)
		(_, Nothing) -> pure (l, r)
		(Just l', Just r') -> get >>= \i -> par @i (l' i) (r' i)

runIteratee :: Eff (Iteratee i ': effs) a -> [i] -> Eff effs (a, [i])
m `runIteratee` ia@(~(i : is)) = case m of
	Pure x -> Pure (x, ia)
	u `Bind` k -> case (decomp u, ia) of
		(Right Get, _ : _) -> k i `runIteratee` is
		(Left u', _) -> u' `Bind` ((`runIteratee` ia) . k)
		(Right Get, []) -> error "not enough inputs"

sample1, sample2 :: Member (Iteratee Integer) effs => Eff effs Integer
sample1 = do
	x <- get
	y <- get
	pure $ x + y

sample2 = do
	x <- get
	y <- get
	z <- get
	pure $ x * y * z
