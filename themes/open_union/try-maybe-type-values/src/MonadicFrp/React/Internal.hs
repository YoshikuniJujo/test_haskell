{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE DataKinds, KindSignatures, TypeOperators, ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts, AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MonadicFrp.React.Internal (
	React(..), EvReqs, EvOccs, Request(..), Firstable, CollapsableOccurred,
	interpretReact, interpretReactSt, adjust, first_, first, done,
	Handle, Handle', HandleSt, HandleSt'
	) where

import Data.Kind

import Data.Or
import Data.Type.Set hiding (Merge)
import Data.UnionSet

import MonadicFrp.ThreadId.Type

type EvReqs (es :: Set Type) = UnionSet es
type EvOccs (es :: Set Type) = UnionSet (Occurred :$: es)

class (Numbered e, Mrgable e) => Request e where data Occurred (e :: Type) :: Type

data React (es :: Set Type) a
	= Done a
	| Await (EvReqs es) (EvOccs es -> ThreadId -> React es (a, ThreadId))
	| Never

instance Functor (React es) where
	f `fmap` Done x = Done $ f x
	f `fmap` Await reqs k = Await reqs \oc ti -> (\(x, ti') -> (f x, ti')) <$> k oc ti
	_ `fmap` Never = Never

instance Applicative (React es) where
	pure = Done
	Done f <*> mx = f <$> mx
	Await reqs kf <*> mx = Await reqs \oc ti -> do
		(f, ti') <- kf oc ti
		r <- f <$> mx
		pure (r, ti')
	Never <*> _ = Never

instance Monad (React es) where
	Done x >>= f = f x
	Await reqs k >>= f = Await reqs \oc ti -> do
		(x, ti') <- k oc ti
		r <- f x
		pure (r, ti')
	Never >>= _ = Never

interpretReact :: Monad m => Handle m es -> React es a -> m a
interpretReact = interpretReact' rootThreadId

interpretReact' :: Monad m => ThreadId -> Handle m es -> React es a -> m a
interpretReact' _ _ (Done x) = pure x
interpretReact' ti p (Await r c) = fst <$> (interpretReact' ti p . (`c` ti) =<< p r)
interpretReact' _ _ Never = error "never occur"			-- <- really?

interpretReactSt :: Monad m => st -> HandleSt st m es -> React es a -> m (a, st)
interpretReactSt = interpretReactSt' rootThreadId

interpretReactSt' :: Monad m => ThreadId -> st -> HandleSt st m es -> React es a -> m (a, st)
interpretReactSt' _ st _ (Done x) = pure (x, st)
interpretReactSt' ti st p (Await r c) = do
	(x, st') <- p st r
	((x', _ti'), st'') <- interpretReactSt' ti st' p $ c x ti
	pure (x', st'')
interpretReactSt' _ _ _ Never = error "never occur"		-- <- really?

adjust :: forall es es' a . (
	(es :+: es') ~ es', Firstable es es', Expandable es es'
	) => React es a -> React es' a
adjust (Done r) = Done r
adjust Never = Never
adjust rct = (rct `first_` (Never :: React es' ())) >>= \case
	(Done x, _) -> pure x
	(rct', _) -> adjust @es @es' rct'

first_ :: forall es es' a b . Firstable es es' =>
	React es a -> React es' b -> React (es :+: es') (React es a, React es' b)
l `first_` r = case (l, r) of
	(Await el _, Await er _) ->
		Await (el `merge` er) \(c :: EvOccs (es :+: es')) ti ->
			let (ti1, ti2) = forkThreadId ti in (, ti) <$>  ud1 l c ti1 `first_` ud2 r c ti2
	(Await el _, Never) ->
		Await (expand el) \(c :: EvOccs (es :+: es')) ti ->
			let (ti1, ti2) = forkThreadId ti in (, ti) <$>  ud1 l c ti1 `first_` ud2 r c ti2
	(Never, Await er _) ->
		Await (expand er) \(c :: EvOccs (es :+: es')) ti ->
			let (ti1, ti2) = forkThreadId ti in (, ti) <$>  ud1 l c ti1 `first_` ud2 r c ti2
	(Done _, _) -> Done (l, r)
	(_, Done _) -> Done (l, r)
	(Never, Never) -> error "bad"
	where
	ud1 = update @es @es'
	ud2 = update @es' @es

infixr 8 `first`

first :: Firstable es es' => React es a -> React es' b -> React (es :+: es') (Or a b)
l `first` r = do
	(l', r') <- l `first_` r
	pure case (done l', done r') of
		(Just l'', Just r'') -> LR l'' r''
		(Just l'', Nothing) -> L l''
		(Nothing, Just r'') -> R r''
		(Nothing, Nothing) -> error "never occur"

update :: forall es es' a . Collapsable (Map Occurred (es :+: es')) (Map Occurred es) =>
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

type Firstable es es' = (
	(es :+: es') ~ (es' :+: es), Mergeable es es' (es :+: es'),
	Expandable es (es :+: es'), Expandable es' (es :+: es'),
	CollapsableOccurred es es', CollapsableOccurred es' es )

type CollapsableOccurred es es' =
	Collapsable (Occurred :$: (es :+: es')) (Occurred :$: es)

type Handle m es = EvReqs es -> m (EvOccs es)
type Handle' m es = EvReqs es -> m (Maybe (EvOccs es))
type HandleSt st m es = st -> EvReqs es -> m (EvOccs es, st)
type HandleSt' st st' m es = st -> EvReqs es -> m (Maybe (EvOccs es), st')
