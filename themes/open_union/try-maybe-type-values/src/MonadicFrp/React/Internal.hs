{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE DataKinds, KindSignatures, TypeOperators, ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts, AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MonadicFrp.React.Internal (
	React(..), EvReqs, EvOccs, Request(..), Firstable, CollapsableOccurred,
	interpretReact, adjust, first_, first, done,
	Handle, Handle'
	) where

import Data.Kind

import Data.Or
import Data.Type.Set hiding (Merge)
import Data.UnionSet

type EvReqs (es :: Set Type) = UnionSet es
type EvOccs (es :: Set Type) = UnionSet (Occurred :$: es)

class Numbered e => Request e where data Occurred (e :: Type) :: Type

data React (es :: Set Type) a
	= Done a
	| Await (EvReqs es) (EvOccs es -> React es a)
	| Never

instance Functor (React es) where
	f `fmap` Done x = Done $ f x
	f `fmap` Await reqs k = Await reqs $ (f <$>) . k
	_ `fmap` Never = Never

instance Applicative (React es) where
	pure = Done
	Done f <*> mx = f <$> mx
	Await reqs kf <*> mx = Await reqs $ (>>= (<$> mx)) . kf
	Never <*> _ = Never

instance Monad (React es) where
	Done x >>= f = f x
	Await reqs k >>= f = Await reqs $ (>>= f) . k
	Never >>= _ = Never

interpretReact :: Monad m => Handle m es -> React es a -> m a
interpretReact _ (Done x) = pure x
interpretReact p (Await r c) = interpretReact p . c =<< p r
interpretReact _ Never = error "never occur"			-- <- really?

adjust :: forall es es' a . (
	(es :+: es') ~ es', Firstable es es', Expandable es es'
	) => React es a -> React es' a
adjust rct@(Await r _) = (rct `first_` (ignore' r :: React es' ())) >>= \case
	(Done x, _) -> pure x
	(rct', _) -> adjust @es @es' rct'
adjust (Done r) = Done r
adjust Never = Never

ignore' :: (Expandable es es') => EvReqs es -> React es' ()
ignore' r = Await (expand r) . const $ ignore' r

first_ :: forall es es' a b . Firstable es es' =>
	React es a -> React es' b -> React (es :+: es') (React es a, React es' b)
l `first_` r = case (l, r) of
	(Await el _, Await er _) ->
		Await (el `merge` er) \(c :: EvOccs (es :+: es')) -> (l `ud1` c) `first_` (r `ud2` c)
	(Await el _, Never) ->
		Await (expand el) \(c :: EvOccs (es :+: es')) -> (l `ud1` c) `first_` (r `ud2` c)
	(Never, Await er _) ->
		Await (expand er) \(c :: EvOccs (es :+: es')) -> (l `ud1` c) `first_` (r `ud2` c)
	(Done _, _) -> Done (l, r)
	(_, Done _) -> Done (l, r)
	(Never, Never) -> error "bad"
	where
	ud1 = update @es @es'
	ud2 = update @es' @es

first :: Firstable es es' => React es a -> React es' b -> React (es :+: es') (Or a b)
l `first` r = do
	(l', r') <- l `first_` r
	pure case (done l', done r') of
		(Just l'', Just r'') -> LR l'' r''
		(Just l'', Nothing) -> L l''
		(Nothing, Just r'') -> R r''
		(Nothing, Nothing) -> error "never occur"

update :: forall es es' a . Collapsable (Map Occurred (es :+: es')) (Map Occurred es) =>
	React es a -> EvOccs (es :+: es') -> React es a
update r@(Await _ c) oc = case collapse oc of
	Just oc' -> c oc'
	Nothing -> r
update Never _ = Never
update (Done _) _ = error "bad: first argument must be Await _ _"

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
