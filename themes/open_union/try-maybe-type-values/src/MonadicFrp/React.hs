{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE DataKinds, KindSignatures, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts, AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MonadicFrp.React where

import Data.Kind

import Data.Sorted hiding (Merge)
import Data.UnionList

type EvReqs (es :: Sorted Type) = UnionList 'False es
type EvOccs (es :: Sorted Type) = UnionList 'True (Occurred :$: es)

class Numbered e => Request e where data Occurred (e :: Type) :: Type

data React (es :: Sorted Type) a =
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
	Nihil es',
	(es :+: es') ~ es',
	(es' :+: es) ~ es',
	Merge es es' es',
	Collapse 'True (Occurred :$: (es :+: es')) (Occurred :$: es),
	Collapse 'True (Occurred :$: (es :+: es')) (Occurred :$: es')
	) =>
	React es a -> React es' a
adjust rct = (rct `first` (ignore :: React es' ())) >>= \case
	(Done x, _) -> pure x
	(rct', _) -> adjust @es @es' rct'

ignore :: Nihil es => React es ()
ignore = Await (expand UnionListNil) $ const ignore

first :: forall es es' a b . (
	(es :+: es') ~ (es' :+: es),
	Merge es es' (es :+: es'),
	Collapse 'True (Occurred :$: (es :+: es')) (Occurred :$: es),
	Collapse 'True (Occurred :$: (es :+: es')) (Occurred :$: es')
	) =>
	React es a -> React es' b -> React (es :+: es') (React es a, React es' b)
l `first` r = case (l, r) of
	(Await el _, Await er _) ->
		Await (el `merge` er) \(c :: EvOccs (es :+: es')) -> (l `ud1` c) `first` (r `ud2` c)
	_ -> Done (l, r)
	where
	ud1 = update @es @es'
	ud2 = update @es' @es

update :: forall es es' a . Collapse 'True (Map Occurred (es :+: es')) (Map Occurred es) =>
	React es a -> EvOccs (es :+: es') -> React es a
update r@(Await _ c) oc = case collapse oc of
	Just oc' -> c oc'
	Nothing -> r
update (Done _) _ = error "bad: first argument must be Await _ _"

before :: (
	(es :+: es') ~ (es' :+: es),
	Merge es es' (es :+: es'),
	Collapse 'True (Occurred :$: (es :+: es')) (Occurred :$: es),
	Collapse 'True (Occurred :$: (es :+: es')) (Occurred :$: es')
	) => React es a -> React es' b -> React (es :+: es') Bool
a `before` b = do
	(a', b') <- a `first` b
	case (done a', done b') of
		(Just _, Nothing) -> pure True
		_ -> pure False

done :: React es a -> Maybe a
done (Done x) = Just x
done (Await _ _) = Nothing

never :: React 'Nil a
never = Await UnionListNil undefined
