{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables, PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE DataKinds, TypeOperators, ConstraintKinds #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances,
	UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Internal.React (
	-- * Class
	Update,
	-- * Constraint
	Firstable, Adjustable,
	-- * Function
	first, adjust, par) where

import Control.Monad.Freer.Par (
	Freer(..), pattern (:=<<), (=<<<), qApp, qAppPar )
import Control.Moffy.Internal.React.Type (
	React, Rct(..), EvOccs, Occurred, ThreadId, forkThreadId, never )
import Data.Type.Set ((:+:), (:$:))
import Data.OneOrMore (
	Expandable, Collapsable, Mergeable, expand, collapse, merge )
import Data.Or (Or(..))

---------------------------------------------------------------------------

-- * FIRST
-- * ADJUST
-- * PAR
-- * UPDATE

---------------------------------------------------------------------------
-- FIRST
---------------------------------------------------------------------------

type Firstable es es' a b = (
	Update a b, Adjustable es (es :+: es'), Adjustable es' (es :+: es'),
	Mergeable (es :+: es') (es :+: es') (es :+: es') )

infixr 8 `first`

first :: Firstable es es' a b =>
	React s es a -> React s es' b -> React s (es :+: es') (Or a b)
(adjust -> l) `first` (adjust -> r) = (<$> l `par` r) \case
	(Pure x, Pure y) -> LR x y; (Pure x, _) -> L x; (_, Pure y) -> R y
	(_ :=<< _, _:=<< _) -> error "never occur"

---------------------------------------------------------------------------
-- ADJUST
---------------------------------------------------------------------------

type Adjustable es es' = (
	Expandable es es', Collapsable (Occurred :$: es') (Occurred :$: es) )

adjust :: Adjustable es es' => React s es a -> React s es' a
adjust = \case
	Pure x -> pure x; _ :=<< Never -> never
	r@(c :=<< Await e) ->
		adjust . maybe r (c `qApp`) . collapse =<<< Await (expand e)
	c :=<< GetThreadId -> adjust . (c `qApp`) =<<< GetThreadId

---------------------------------------------------------------------------
-- PAR
---------------------------------------------------------------------------

par :: (Update a b, Mergeable es es es) =>
	React s es a -> React s es b -> React s es (React s es a, React s es b)
l `par` r = case (l, r) of
	(_ :=<< Never, _ :=<< Never) -> never
	(Pure _, _) -> pure (l, r); (_, Pure _) -> pure (l, r)
	(_ :=<< Never, _) -> pure (l, r); (_, _ :=<< Never) -> pure (l, r)
	(c :=<< GetThreadId, _) -> (`par` r) . qApp c . fst =<< forkThreadId
	(_, c' :=<< GetThreadId) -> (l `par`) . qApp c' . snd =<< forkThreadId
	(_ :=<< Await el, _ :=<< Await er) -> forkThreadId >>= \(t, u) ->
		uncurry par . update l t r u =<< pure =<<< Await (el `merge` er)

---------------------------------------------------------------------------
-- UPDATE
---------------------------------------------------------------------------

class Update a b where
	update ::
		React s es a -> ThreadId -> React s es b -> ThreadId ->
		EvOccs es -> (React s es a, React s es b)

instance Update a a where
	update (c :=<< GetThreadId) t r u b = update (c `qApp` t) t r u b
	update l t (c' :=<< GetThreadId) u b = update l t (c' `qApp` u) u b
	update l@(_ :=<< Never) _ (c' :=<< Await _) _ b = (l, c' `qApp` b)
	update (c :=<< Await _) _ r@(_ :=<< Never) _ b = (c `qApp` b, r)
	update (c :=<< Await _) _ (c' :=<< Await _) _ b = qAppPar c c' b
	update l _ r _ _ = (l, r)

instance {-# OVERLAPPABLE #-} Update a b where
	update (c :=<< GetThreadId) t r u b = update (c `qApp` t) t r u b
	update l t (c' :=<< GetThreadId) u b = update l t (c' `qApp` u) u b
	update l@(_ :=<< Never) _ (c' :=<< Await _) _ b = (l, c' `qApp` b)
	update (c :=<< Await _) _ r@(_ :=<< Never) _ b = (c `qApp` b, r)
	update (c :=<< Await _) _ (c' :=<< Await _) _ b = (c `qApp` b, c' `qApp` b)
	update l _ r _ _ = (l, r)
