{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE DataKinds, KindSignatures, TypeOperators, ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts, AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MonadicFrp.React.Internal (
	React(..), EvReqs, EvOccs, Request(..), First, CollapsableOccurred,
	Or(..),
	interpret, adjust,
	first_, first,
	done, never,
	) where

import Data.Kind

import Data.Or
import Data.Type.Set hiding (Merge)
import Data.UnionSet

type EvReqs (es :: Set Type) = UnionSet 'False es
type EvOccs (es :: Set Type) = UnionSet 'True (Occurred :$: es)

class Numbered e => Request e where data Occurred (e :: Type) :: Type

data React (es :: Set Type) a =
	Done a | Await (EvReqs es) (EvOccs es -> React es a)

instance Functor (React es) where
	f `fmap` Done x = Done $ f x
	f `fmap` Await reqs k = Await reqs $ (f <$>) . k

instance Applicative (React es) where
	pure = Done
	Done f <*> mx = f <$> mx
	Await reqs kf <*> mx = Await reqs $ (>>= (<$> mx)) . kf

instance Monad (React es) where
	Done x >>= f = f x
	Await reqs k >>= f = Await reqs $ (>>= f) . k

interpret :: Monad m => (EvReqs es -> m (EvOccs es)) -> React es a -> m a
interpret _ (Done x) = pure x
interpret p (Await r c) = interpret p . c =<< p r

adjust :: forall es es' a . (
	Nihil es', (es :+: es') ~ es', First es es'
	) => React es a -> React es' a
adjust rct = (rct `first_` (ignore :: React es' ())) >>= \case
	(Done x, _) -> pure x
	(rct', _) -> adjust @es @es' rct'

ignore :: Nihil es => React es ()
ignore = Await (expand Empty) $ const ignore

first_ :: forall es es' a b . First es es' =>
	React es a -> React es' b -> React (es :+: es') (React es a, React es' b)
l `first_` r = case (l, r) of
	(Await el _, Await er _) ->
		Await (el `merge` er) \(c :: EvOccs (es :+: es')) -> (l `ud1` c) `first_` (r `ud2` c)
	_ -> Done (l, r)
	where
	ud1 = update @es @es'
	ud2 = update @es' @es

first :: First es es' => React es a -> React es' b -> React (es :+: es') (Or a b)
l `first` r = do
	(l', r') <- l `first_` r
	pure case (done l', done r') of
		(Just l'', Just r'') -> LR l'' r''
		(Just l'', Nothing) -> L l''
		(Nothing, Just r'') -> R r''
		(Nothing, Nothing) -> error "never occur"

update :: forall es es' a . Collapsable 'True (Map Occurred (es :+: es')) (Map Occurred es) =>
	React es a -> EvOccs (es :+: es') -> React es a
update r@(Await _ c) oc = case collapse oc of
	Just oc' -> c oc'
	Nothing -> r
update (Done _) _ = error "bad: first argument must be Await _ _"

done :: React es a -> Maybe a
done (Done x) = Just x
done (Await _ _) = Nothing

never :: React 'Nil a
never = Await Empty undefined

type First es es' = (
	(es :+: es') ~ (es' :+: es), Mergeable es es' (es :+: es'),
	CollapsableOccurred es es', CollapsableOccurred es' es )

type CollapsableOccurred es es' =
	Collapsable 'True (Occurred :$: (es :+: es')) (Occurred :$: es)
