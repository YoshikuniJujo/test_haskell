{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE DataKinds, KindSignatures, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts, AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module React (
	-- * Types
	React(..), EvReqs, EvOccs, Request(Occurred),
	-- * Run React
	interpret,
	-- * Type Conversion
	adjust,
	-- * Conversion
	done,
	-- * Parrallel Composition
	first, before,
	-- * Others
	never ) where

import Prelude hiding (head, tail, filter)

import Data.Kind
import Data.Maybe
import Data.List.NonEmpty

import OpenUnionValue
import Sorted

type EvReqs (es :: Sorted Type) = [UnionValue es]
type EvOccs (es :: Sorted Type) = NonEmpty (UnionValue (Occurred :$: es))

class Numbered e => Request e where data Occurred (e :: Type) :: Type

data React (es :: Sorted Type) a =
	Done a | Await (EvReqs es) (EvOccs es -> React es a)

instance Functor (React es) where
	f `fmap` Done x = Done $ f x
	f `fmap` Await reqs k = Await reqs \occs -> f `fmap` k occs

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
	(es :+: es') ~ es', (es' :+: es) ~ es', Convert es es', Convert es' es',
	Convert (Occurred :$: es') (Occurred :$: es),
	Convert (Occurred :$: es) (Occurred :$: es'),
	Convert (Occurred :$: es') (Occurred :$: es') ) => React es a -> React es' a
adjust rct = (rct `first` (ignore :: React es' ())) >>= \case
	(Done x, _) -> pure x
	(rct', _) -> adjust rct'

ignore :: React es ()
ignore = Await [] $ const ignore

first :: forall es es' a b . (
	(es :+: es') ~ (es' :+: es),
	Convert es (es' :+: es),
	Convert es' (es' :+: es),
	Convert (Occurred :$: es' :+: es) (Occurred :$: es),
	Convert (Occurred :$: es' :+: es) (Occurred :$: es')
	) => React es a -> React es' b -> React (es :+: es') (React es a, React es' b)
l `first` r = case (l, r) of
	(Await el _, Await er _) ->
		Await ((fromJust . convert <$> el) ++ (fromJust . convert <$> er))
			\(c :: EvOccs (es :+: es')) -> (l `ud1` c) `first` (r `ud2` c)
	_ -> Done (l, r)
	where
	ud1 = update @es @es'
	ud2 = update @es' @es

update :: Convert (Occurred :$: es :+: es') (Occurred :$: es) => React es a -> EvOccs (es :+: es') -> React es a
update r@(Await _ c) oc = case fromJust <$> filter isJust (convert <$> oc) of
	o : os -> c $ o :| os
	[] -> r
update _ _ = error "bad: first argument must be Await _ _"

before :: (
	(es :+: es') ~ (es' :+: es), Convert es (es' :+: es), Convert es' (es' :+: es),
	Convert (Occurred :$: es' :+: es) (Occurred :$: es),
	Convert (Occurred :$: es' :+: es) (Occurred :$: es') ) =>
	React es a -> React es' b -> React (es :+: es') Bool
before a b = do
	(a', b') <- a `first` b
	case (done a', done b') of
		(Just _, Nothing) -> pure True
		_ -> pure False

done :: React es a -> Maybe a
done (Done x) = Just x
done _ = Nothing

never :: React 'Nil a
never = Await [] undefined
