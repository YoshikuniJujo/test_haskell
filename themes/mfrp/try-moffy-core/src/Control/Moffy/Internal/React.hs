{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE TypeOperators, ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances,
	UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Internal.React (
	-- * Class
	Adjustable, Updatable,
	-- * Constraint Synonym
	Firstable,
	-- * Function
	adjust, first_, par) where

import Control.Arrow ((***))
import Control.Monad.Freer.Par (
	pattern Pure, pattern (:=<<), (=<<<), app, appPar )
import Control.Moffy.Internal.React.Type (
	React, Rct(..), EvOccs, CollapsableOccurred, ThreadId, never )
import Data.Type.Set ((:+:))
import Data.OneOrMore (Expandable, Mergeable, expand, merge)
import Data.Or (Or(..))

import Data.OneOrMoreApp (collapse)

---------------------------------------------------------------------------

-- * FIRST
-- * ADJUST
-- * PAR
-- * UPDATABLE

---------------------------------------------------------------------------
-- FIRST
---------------------------------------------------------------------------

type Firstable es es' a b = (
	Updatable a b, Adjustable es (es :+: es'), Adjustable es' (es :+: es'),
	((es :+: es') :+: (es :+: es')) ~ (es :+: es'),
	Mergeable (es :+: es') (es :+: es') (es :+: es') )

first_ :: Firstable es es' a b =>
	React s (es :+: es') (ThreadId, ThreadId) ->
	React s es a -> React s es' b -> React s (es :+: es') (Or a b)
first_ ft (adjust -> l) (adjust -> r) = (<$> par ft l r) \case
	(Pure x, Pure y) -> LR x y; (Pure x, _) -> L x; (_, Pure y) -> R y
	(_ :=<< _, _:=<< _) -> error "never occur"

---------------------------------------------------------------------------
-- ADJUST
---------------------------------------------------------------------------

class Adjustable es es' where adjust :: React s es a -> React s es' a
instance Adjustable es es where adjust = id
instance {-# OVERLAPPABLE #-} (Expandable es es', CollapsableOccurred es' es) =>
	Adjustable es es' where adjust = adj

adj :: (Expandable es es', CollapsableOccurred es' es) =>
	React s es a -> React s es' a
adj = \case
	Pure x -> pure x; _ :=<< Never -> never
	c :=<< GetThreadId -> adj . (c `app`) =<<< GetThreadId
	r@(c :=<< Await e) ->
		adj . maybe r (c `app`) . collapse =<<< Await (expand e)

---------------------------------------------------------------------------
-- PAR
---------------------------------------------------------------------------

par :: (Updatable a b, Mergeable es es es, (es :+: es) ~ es) =>
	React s es (ThreadId, ThreadId) ->
	React s es a -> React s es b -> React s es (React s es a, React s es b)
par ft l r = case (l, r) of
	(Pure _, _) -> pure (l, r); (_, Pure _) -> pure (l, r)
	(_ :=<< Never, _ :=<< Never) -> never
	(_ :=<< Never, _) -> (never ,) . pure <$> r
	(_, _ :=<< Never) -> (, never) . pure <$> l
	(c :=<< GetThreadId, c' :=<< GetThreadId) ->
		uncurry (par ft) . (app c *** app c') =<< ft
	(c :=<< GetThreadId, _) -> flip (par ft) r . app c . fst =<< ft
	(_, c' :=<< GetThreadId) -> par ft l . app c' . snd =<< ft
	(_ :=<< Await el, _ :=<< Await er) -> ft >>= \(t, u) ->
		uncurry (par ft)
			. update l t r u =<< pure =<<< Await (el `merge` er)

---------------------------------------------------------------------------
-- UPDATABLE
---------------------------------------------------------------------------

class Updatable a b where
	update :: React s es a -> ThreadId -> React s es b -> ThreadId ->
		EvOccs es -> (React s es a, React s es b)

instance Updatable a a where
	update (c :=<< GetThreadId) t (c' :=<< GetThreadId) u x =
		update (c `app` t) t (c' `app` u) u x
	update (c :=<< GetThreadId) t r u x = update (c `app` t) t r u x
	update l t (c' :=<< GetThreadId) u x = update l t (c' `app` u) u x
	update l@(_ :=<< Never) _ (c' :=<< Await _) _ x = (l, c' `app` x)
	update (c :=<< Await _) _ r@(_ :=<< Never) _ x = (c `app` x, r)
	update (c :=<< Await _) _ (c' :=<< Await _) _ x = appPar c c' x
	update l _ r _ _ = (l, r)

instance {-# OVERLAPPABLE #-} Updatable a b where
	update (c :=<< GetThreadId) t (c' :=<< GetThreadId) u x =
		update (c `app` t) t (c' `app` u) u x
	update (c :=<< GetThreadId) t r u x = update (c `app` t) t r u x
	update l t (c' :=<< GetThreadId) u x = update l t (c' `app` u) u x
	update l@(_ :=<< Never) _ (c' :=<< Await _) _ x = (l, c' `app` x)
	update (c :=<< Await _) _ r@(_ :=<< Never) _ x = (c `app` x, r)
	update (c :=<< Await _) _ (c' :=<< Await _) _ x = (c `app` x, c' `app` x)
	update l _ r _ _ = (l, r)
