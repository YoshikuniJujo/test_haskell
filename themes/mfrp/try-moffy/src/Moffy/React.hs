{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GADTs, TypeFamilies, ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Moffy.React where

import Data.Type.Set
import Data.OneOrMore
import Data.Or

import Freer

import Moffy.React.Common

first :: (
	Expandable es (es :+: es'), Expandable es' (es :+: es'),
	Update es a es' b,
	Mergeable es es' (es :+: es')
	) => React s es a -> React s es' b -> React s (es :+: es') (Or a b)
l `first` r = (<$> l `par` r) \case
	(Pure l', Pure r') -> LR l' r'
	(Pure l', _) -> L l'; (_, Pure r') -> R r'
	(Await _ :>>= _, _) -> error "(Await _ :>>= _, _)"
	(_, Await _ :>>= _) -> error "(_, Await _ :>>= _)"
	(Never :>>= _, _) -> error "(Never, _)"
	(_, Never :>>= _) -> error "(_, Never)"
	(GetThreadId :>>= _, _) -> error "(GetThreadId :>>= _, _)"
	(_, GetThreadId :>>= _) -> error "(_, GetThreadId :>>= _)"
	_ -> error "never occur"

type Parable es a es' b = (
	Update es a es' b, Mergeable es es' (es :+: es'),
	Expandable es (es :+: es'), Expandable es' (es :+: es') )

par ::	Parable es a es' b =>
	React s es a -> React s es' b -> React s (es :+: es') (React s es a, React s es' b)
l `par` r = case (l, r) of
	(Never :>>= _, Never :>>= _) -> error "never end"
	(GetThreadId :>>= c, r') -> do
		ti <- getThreadId
		let	(ti1, _ti2) = forkThreadId ti
		(c `qApp` ti1) `par` r'
	(rct, GetThreadId :>>= c') -> do
		ti <- getThreadId
		let	(_ti1, ti2) = forkThreadId ti
		rct `par` (c' `qApp` ti2)
	(Pure _, _) -> pure (l, r)
	(_, Pure _) -> pure (l, r)
--	(Never, Await er :>>= c) 
	(Await el :>>= _, Await er :>>= _) -> let
		e = el `merge` er
		c b ti = let
			(ti1, ti2) = forkThreadId ti
			(u, u') = update l ti1 r ti2 b in u `par` u' in do
			o <- Await e >>>= pure
			ti <- getThreadId
			c o ti
--		Await e >>>= c
	(Await el :>>= _, _) -> let
		e = expand el
		c b ti = let
			(ti1, ti2) = forkThreadId ti
			(u, u') = update l ti1 r ti2 b in u `par` u' in do
			o <- Await e >>>= pure
			ti <- getThreadId
			c o ti
	(_, Await er :>>= _) -> let
		e = expand er
		c b ti = let
			(ti1, ti2) = forkThreadId ti
			(u, u') = update l ti1 r ti2 b in u `par` u' in do
			o <- Await e >>>= pure
			ti <- getThreadId
			c o ti
	_ -> Pure (l, r)

type Updatable es a es' b = (
	CollapsableOccurred (es :+: es') es, CollapsableOccurred (es :+: es') es',
	(Occurred :$: es) ~ (Occurred :$: es :+: es) )

class (	CollapsableOccurred (es :+: es') es, CollapsableOccurred (es :+: es') es',
	(Occurred :$: es) ~ (Occurred :$: es :+: es) ) =>
	Update es a es' b where
	update ::
		React s es a -> ThreadId ->
		React s es' b -> ThreadId ->
		EvOccs (es :+: es') -> (React s es a, React s es' b)

instance {-# OVERLAPPABLE #-} Updatable es a es' b => Update es a es' b where
	update (GetThreadId :>>= c) ti r' ti' b = update (c `qApp` ti) ti r' ti' b
	update r ti (GetThreadId :>>= c') ti' b = update r ti (c' `qApp` ti') ti' b
	update r@(Await _ :>>= c) _ r'@(Await _ :>>= c') _ b = case (collapse b, collapse b) of
		(Just b', Just b'') -> (c `qApp` b', c' `qApp` b'')
		(Just b', Nothing) -> (c `qApp` b', r')
		(Nothing, Just b'') -> (r, c' `qApp` b'')
		(Nothing, Nothing) -> (r, r')
	update r@(Never :>>= _) _ r'@(Await _ :>>= c') _ b = case collapse b of
		Just b'' -> (r, c' `qApp` b'')
		Nothing -> (r, r')
	update r@(Await _ :>>= c) _ r'@(Never :>>= _) _ b = case collapse b of
		Just b' -> (c `qApp` b', r')
		Nothing -> (r, r')
	update r _ r' _ _ = (r, r')

instance Updatable es a es a => Update es a es a where
	update (GetThreadId :>>= c) ti r' ti' b = update (c `qApp` ti) ti r' ti' b
	update r ti (GetThreadId :>>= c') ti' b = update r ti (c' `qApp` ti') ti' b
	update (Await _ :>>= c) _ (Await _ :>>= c') _ b = qAppPar c c' b
	update r _ r' _ _ = (r, r')

update' :: (
	CollapsableOccurred (es :+: es') es, CollapsableOccurred (es :+: es') es'
	) =>
	React s es a -> ThreadId ->
	React s es' b -> ThreadId ->
	EvOccs (es :+: es') -> (React s es a, React s es' b)
update' (GetThreadId :>>= c) ti r' ti' b = update' (c `qApp` ti) ti r' ti' b
update' r ti (GetThreadId :>>= c') ti' b = update' r ti (c' `qApp` ti') ti' b
update' r@(Await _ :>>= c) _ r'@(Await _ :>>= c') _ b = case (collapse b, collapse b) of
		(Just b', Just b'') -> (c `qApp` b', c' `qApp` b'')
		(Just b', Nothing) -> (c `qApp` b', r')
		(Nothing, Just b'') -> (r, c' `qApp` b'')
		(Nothing, Nothing) -> (r, r')
update' r@(Never :>>= _) _ r'@(Await _ :>>= c') _ b = case collapse b of
	Just b'' -> (r, c' `qApp` b'')
	Nothing -> (r, r')
update' r@(Await _ :>>= c) _ r'@(Never :>>= _) _ b = case collapse b of
	Just b' -> (c `qApp` b', r')
	Nothing -> (r, r')
update' r _ r' _ _ = (r, r')

par' ::	(
	CollapsableOccurred (es :+: es') es, CollapsableOccurred (es :+: es') es',
	Mergeable es es' (es :+: es'),
	Expandable es (es :+: es'), Expandable es' (es :+: es')
	) => React s es a -> React s es' b -> React s (es :+: es') (React s es a, React s es' b)
l `par'` r = case (l, r) of
	(Await el :>>= _, Await er :>>= _) -> let
		e = el `merge` er
		c b ti = let (u, u') = update' l ti r ti b in u `par'` u' in
		Await e >>>= \b -> c b =<< getThreadId
	(_ :>>= _, Await er :>>= _) -> let
		e = expand er
		c b ti = let (u, u') = update' l ti r ti b in u `par'` u' in
		Await e >>>= \b -> c b =<< getThreadId
	(Await el :>>= _, _ :>>= _) -> let
		e = expand el
		c b ti = let (u, u') = update' l ti r ti b in u `par'` u' in
		Await e >>>= \b -> c b =<< getThreadId
	(GetThreadId :>>= c, r') -> do
		ti <- getThreadId
		(c `qApp` ti) `par'` r'
	_ -> Pure (l, r)

type Adjustable es es' = (
	(es :+: es') ~ es', Mergeable es es' es',
	CollapsableOccurred es' es, CollapsableOccurred es' es',
	Expandable es es', Expandable es' es' )

adjust :: forall s es es' a . Adjustable es es' => React s es a -> React s es' a
adjust = \case
	Pure x -> pure x
	(Never :>>= _) -> Never >>>= pure
--	GetThreadId :>>= c -> GetThreadId :>>= adjust c
--	r@(Await _ :>>= _) -> (r `par'` (Never >>>= pure :: React s es' b)) >>= \case
	r -> (r `par'` (Never >>>= pure :: React s es' b)) >>= \case
		(Pure x, _) -> pure x
		(Await _ :>>= _, _) -> error "Await _ _"
		(GetThreadId :>>= _, _) -> error "GetThreadId"
		_ -> error "never occur"
