{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE DataKinds, TypeOperators, ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts, AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MonadicFrp.React.Internal (
	-- * Type
	React(..), EvReqs, EvOccs, Request(..),
	Adjustable, Firstable, CollapsableOccurred,
	-- * Handle
	Handle, Handle', HandleSt, HandleSt',
	-- * Interpret
	interpretReact, interpretReactSt,
	-- * Combinator
	adjust, first, par ) where

import Data.Kind (Type)
import Data.Type.Set (Set, Numbered, (:+:), (:$:))
import Data.UnionSet (
	UnionSet, Mrgable, Mergeable, Expandable, Collapsable,
	merge, expand, collapse )
import Data.Or (Or(..))

import qualified Control.Arrow as A

import MonadicFrp.ThreadId.Type (ThreadId, rootThreadId, forkThreadId)

---------------------------------------------------------------------------

-- * TYPE REACT
--	+ TYPE DEFINITION
--	+ MONAD
-- * HANDLE
-- * INTERPRET
-- * COMBINATOR
--	+ CONSTRAINT SYNONYM
--	+ FUNCTION

---------------------------------------------------------------------------
-- TYPE REACT
---------------------------------------------------------------------------

-- TYPE DEFINITION

type EvReqs (es :: Set Type) = UnionSet es
type EvOccs (es :: Set Type) = UnionSet (Occurred :$: es)

class (Numbered e, Mrgable e) => Request e where
	data Occurred (e :: Type) :: Type

data React (es :: Set Type) a
	= Never | Done a
	| Await (EvReqs es) (EvOccs es -> ThreadId -> React es (a, ThreadId))

react :: b -> (a -> b) ->
	(EvReqs es -> (EvOccs es -> ThreadId -> React es (a, ThreadId)) -> b) ->
	React es a -> b
react n d a = \case Never -> n; Done x -> d x; Await rqs k -> a rqs k

-- MONAD

instance Functor (React es) where
	fmap f = react Never (Done . f)
		\rqs k -> Await rqs \oc ti -> A.first f <$> k oc ti

instance Applicative (React es) where
	pure = Done
	mf <*> mx = ($ mf) $ react Never (<$> mx) \rqs kf ->
		Await rqs \oc ti -> kf oc ti >>= \(f, ti') -> (, ti') . f <$> mx

instance Monad (React es) where
	m >>= f = ($ m) $ react Never f \rqs k ->
		Await rqs \oc ti -> k oc ti >>= \(x, ti') -> (, ti') <$> f x

---------------------------------------------------------------------------
-- HANDLE
---------------------------------------------------------------------------

type Handle m es = EvReqs es -> m (EvOccs es)
type Handle' m es = EvReqs es -> m (Maybe (EvOccs es))
type HandleSt st m es = st -> EvReqs es -> m (EvOccs es, st)
type HandleSt' st st' m es = st -> EvReqs es -> m (Maybe (EvOccs es), st')

---------------------------------------------------------------------------
-- INTERPRET
---------------------------------------------------------------------------

interpretReact :: forall m es a . Monad m => Handle m es -> React es a -> m a
interpretReact hdl = go where
	go :: React es b -> m b
	go = react (error "never end") pure \rqs k ->
		fst <$> (go . (`k` rootThreadId) =<< hdl rqs)

interpretReactSt :: forall st m es a . Monad m =>
	st -> HandleSt st m es -> React es a -> m (a, st)
interpretReactSt st0 hdl = go st0 where
	go :: st -> React es b -> m (b, st)
	go st = react (error "never end") (pure . (, st)) \rqs k -> do
		(x, st') <- hdl st rqs
		(fst `A.first`) <$> go st' (k x rootThreadId)

---------------------------------------------------------------------------
-- COMBINATOR
---------------------------------------------------------------------------

-- CONSTRAINT SYNONYM

type Adjustable es es' =
	((es :+: es') ~ es', Expandable es es', Firstable es es')

type Firstable es es' = (
	(es :+: es') ~ (es' :+: es), Mergeable es es' (es :+: es'),
	Expandable es (es :+: es'), Expandable es' (es :+: es'),
	CollapsableOccurred es es', CollapsableOccurred es' es )

type CollapsableOccurred es es' =
	Collapsable (Occurred :$: (es :+: es')) (Occurred :$: es)

-- FUNCTION

adjust :: forall es es' a . Adjustable es es' => React es a -> React es' a
adjust = \case
	Never -> Never; Done x -> pure x
	r@(Await _ _) -> (r `par` (Never :: React es' ())) >>= \case
		(Done x, _) -> pure x; _ -> error "never occur"

infixr 8 `first`

first :: Firstable es es' => React es a -> React es' b -> React (es :+: es') (Or a b)
l `first` r = do
	(l', r') <- l `par` r
	pure case (done l', done r') of
		(Just l'', Just r'') -> LR l'' r''
		(Just l'', Nothing) -> L l''
		(Nothing, Just r'') -> R r''
		(Nothing, Nothing) -> error "never occur"

par :: forall es es' a b . Firstable es es' =>
	React es a -> React es' b -> React (es :+: es') (React es a, React es' b)
l `par` r = case (l, r) of
	(Await el _, Await er _) ->
		Await (el `merge` er) \(c :: EvOccs (es :+: es')) ti ->
			let (ti1, ti2) = forkThreadId ti in (, ti) <$>  ud1 l c ti1 `par` ud2 r c ti2
	(Await el _, Never) ->
		Await (expand el) \(c :: EvOccs (es :+: es')) ti ->
			let (ti1, ti2) = forkThreadId ti in (, ti) <$>  ud1 l c ti1 `par` ud2 r c ti2
	(Never, Await er _) ->
		Await (expand er) \(c :: EvOccs (es :+: es')) ti ->
			let (ti1, ti2) = forkThreadId ti in (, ti) <$>  ud1 l c ti1 `par` ud2 r c ti2
	(Done _, _) -> Done (l, r)
	(_, Done _) -> Done (l, r)
	(Never, Never) -> error "never end"
	where
	ud1 = update @es @es'
	ud2 = update @es' @es

update :: forall es es' a . Collapsable (Occurred :$: (es :+: es')) (Occurred :$: es) =>
	React es a -> EvOccs (es :+: es') -> ThreadId -> React es a
update r@(Await _ c) oc ti = case collapse oc of
	Just oc' -> fst <$> c oc' ti
	Nothing -> r
update Never _ _ = Never
update (Done _) _ _ = error "bad: first argument must be Await _ _"

done :: React es a -> Maybe a
done (Done x) = Just x
done (Await _ _) = Nothing
done Never = Nothing
