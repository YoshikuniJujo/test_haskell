{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables, PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE DataKinds, TypeOperators, ConstraintKinds #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances,
	UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Internal.React (
	-- * Type
	Update, Adjustable, Firstable,
	-- * Function
	adjust, first, par, update ) where

import Control.Monad.Freer.Par (
	Freer(..), pattern (:>>=), (>>>=), (=<<<), qApp, qAppPar )
import Data.Type.Set ((:+:))
import Data.OneOrMore (Expandable, Mergeable, expand, collapse, merge)
import Data.Or (Or(..))

import Control.Moffy.Internal.React.Type (
	React, Rct(..), EvOccs, CollapsableOccurred,
	ThreadId, forkThreadId, never )

---------------------------------------------------------------------------

-- * ADJUST
-- * FIRST
-- * PAR
-- * UPDATE

---------------------------------------------------------------------------
-- ADJUST
---------------------------------------------------------------------------

type Adjustable es es' = (Expandable es es', CollapsableOccurred es' es)

adjust :: Adjustable es es' => React s es a -> React s es' a
adjust = \case
	Pure x -> pure x
	Never :>>= _ -> Never >>>= pure
	r@(Await e :>>= c) ->
		Await (expand e) >>>= adjust . maybe r (c `qApp`) . collapse
	GetThreadId :>>= c -> GetThreadId >>>= adjust . (c `qApp`)

---------------------------------------------------------------------------
-- FIRST
---------------------------------------------------------------------------

type Firstable es es' a b = (
	Update a b, Adjustable es (es :+: es'), Adjustable es' (es :+: es'),
	Mergeable (es :+: es') (es :+: es') (es :+: es') )

first :: Firstable es es' a b =>
	React s es a -> React s es' b -> React s (es :+: es') (Or a b)
(adjust -> l) `first` (adjust -> r) = (<$> l `par` r) \case
	(Pure x, Pure y) -> LR x y; (Pure x, _) -> L x; (_, Pure y) -> R y
	(_ :>>= _, _:>>= _) -> error "never occur"

---------------------------------------------------------------------------
-- PAR
---------------------------------------------------------------------------

par :: (Update a b, Mergeable es es es) =>
	React s es a -> React s es b -> React s es (React s es a, React s es b)
l `par` r = case (l, r) of
	(Never :>>= _, Never :>>= _) -> never
	(Pure _, _) -> pure (l, r); (_, Pure _) -> pure (l, r)
	(Never :>>= _, _) -> (never ,) . Pure <$> r
	(_, Never :>>= _) -> (, never) . Pure <$> l
	(GetThreadId :>>= c, _) -> (`par` r) =<< qApp c . fst <$> forkThreadId
	(_, GetThreadId :>>= c') -> (l `par`) =<< qApp c' . snd <$> forkThreadId
	(Await el :>>= _, Await er :>>= _) -> forkThreadId >>= \(t, u) ->
		uncurry par . update l t r u =<< pure =<<< Await (el `merge` er)

---------------------------------------------------------------------------
-- UPDATE
---------------------------------------------------------------------------

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
