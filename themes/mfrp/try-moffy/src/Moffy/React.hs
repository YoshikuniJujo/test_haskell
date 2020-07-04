{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE GADTs, TypeFamilies, ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Moffy.React (
	Update, Adjustable, Firstable, adjust, first, par, update ) where

import Data.Type.Set
import Data.OneOrMore
import Data.Or

import Freer

import Moffy.React.Common

class Update a b where
	update ::
		React s es a -> ThreadId ->
		React s es b -> ThreadId ->
		EvOccs es -> (React s es a, React s es b)

instance {-# OVERLAPPABLE #-} Update a b where
	update (GetThreadId :>>= c) ti r' ti' b = update (c `qApp` ti) ti r' ti' b
	update r ti (GetThreadId :>>= c') ti' b = update r ti (c' `qApp` ti') ti' b
	update (Await _ :>>= c) _ (Await _ :>>= c') _ b = (c `qApp` b, c' `qApp` b)
	update r@(Never :>>= _) _ (Await _ :>>= c') _ b = (r, c' `qApp` b)
	update (Await _ :>>= c) _ r'@(Never :>>= _) _ b = (c `qApp` b, r')
	update r _ r' _ _ = (r, r')

instance Update a a where
	update (GetThreadId :>>= c) ti r' ti' b = update (c `qApp` ti) ti r' ti' b
	update r ti (GetThreadId :>>= c') ti' b = update r ti (c' `qApp` ti') ti' b
	update (Await _ :>>= c) _ (Await _ :>>= c') _ b = qAppPar c c' b
	update r _ r' _ _ = (r, r')

type Adjustable es es' = (Expandable es es', CollapsableOccurred es' es)

adjust :: Adjustable es es' => React s es a -> React s es' a
adjust = \case
	Pure x -> pure x
	Never :>>= _ -> Never >>>= pure
	l@(Await e :>>= c) -> Await (expand e) >>>= \occ -> case collapse occ of
		Just occ' -> adjust $ c `qApp` occ'
		Nothing -> adjust l
	GetThreadId :>>= c -> GetThreadId >>>= \ti -> adjust $ c `qApp` ti

type Firstable es es' a b = (
	Update a b, Adjustable es (es :+: es'), Adjustable es' (es :+: es'),
	Mergeable (es :+: es') (es :+: es') (es :+: es') )

first :: Firstable es es' a b =>
	React s es a -> React s es' b -> React s (es :+: es') (Or a b)
l `first` r = (<$> adjust l `par` adjust r) \case
	(Pure x, Pure y) -> LR x y
	(Pure x, _) -> L x; (_, Pure y) -> R y
	(_ :>>= _, _:>>= _) -> error "never occur"

par :: (Update a b, Mergeable es es es) =>
	React s es a -> React s es b -> React s es (React s es a, React s es b)
l `par` r = case (l, r) of
	(Never :>>= _, Never :>>= _) -> never
	(Pure _, _) -> pure (l, r)
	(_, Pure _) -> pure (l, r)
	(Never :>>= _, _) -> (never ,) . Pure <$> r
	(_, Never :>>= _) -> (, never) . Pure <$> l
	(GetThreadId :>>= c, r') -> do
		ti <- getThreadId
		let	(ti1, _ti2) = forkThreadId ti
		(c `qApp` ti1) `par` r'
	(l', GetThreadId :>>= c') -> do
		ti <- getThreadId
		let	(_ti1, ti2) = forkThreadId ti
		l' `par` (c' `qApp` ti2)
	(Await el :>>= _, Await er :>>= _) -> let
		e = el `merge` er
		c b ti = let
			(ti1, ti2) = forkThreadId ti
			(u, u') = update l ti1 r ti2 b in u `par` u' in do
			o <- Await e >>>= pure
			ti <- getThreadId
			c o ti
