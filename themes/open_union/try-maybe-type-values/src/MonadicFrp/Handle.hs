{-# LANGUAGE LambdaCase, TupleSections #-}
{-# LANGUAGE DataKinds, TypeOperators, ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-orphans #-}

module MonadicFrp.Handle (
	-- * Types
	Handle, Handle', HandleSt, HandleSt', Beforable,
	-- * Composer
	retry, before, merge, retrySt, expandHandleSt, mergeHandleSt
	) where

import Prelude hiding (map, repeat, scanl, until)

import Control.Arrow
import Data.Type.Set
import Data.UnionSet hiding (merge)
import MonadicFrp.React hiding (first)

retry :: Monad m => Handle' m es -> Handle m es
retry h reqs = h reqs >>= \case
	Just occs -> pure occs; Nothing -> retry h reqs

retrySt :: Monad m => HandleSt' st st m es -> HandleSt st m es
retrySt h st reqs = h st reqs >>= \case
	(Just occs, st') -> pure (occs, st')
	(Nothing, st') -> retrySt h st' reqs

type Beforable es es' = (
	Collapsable (es :+: es') es, Collapsable (es :+: es') es',
	Expandable (Occurred :$: es) (Occurred :$: es :+: es'),
	Expandable (Occurred :$: es') (Occurred :$: es :+: es') )

infixr 5 `before`

before :: (Monad m, Beforable es es') => Handle' m es -> Handle' m es' -> Handle' m (es :+: es')
before h1 h2 reqs = maybe (pure Nothing) h1 (collapse reqs) >>= \case
	Just occs -> pure . Just $ expand occs
	Nothing -> (expand <$>) <$> maybe (pure Nothing) h2 (collapse reqs)

infixr 6 `merge`

merge :: (
	Monad m, Beforable es es',
	Mergeable (Occurred :$: es) (Occurred :$: es') (Occurred :$: es :+: es')
	) => Handle' m es -> Handle' m es' -> Handle' m (es :+: es')
merge h1 h2 reqs = do
	r1 <- maybe (pure Nothing) h1 $ collapse reqs
	r2 <- maybe (pure Nothing) h2 $ collapse reqs
	pure $ r1 `merge'` r2

expandHandleSt :: (
	Monad m, Collapsable es' es,
	Expandable (Occurred :$: es) (Occurred :$: es')) =>
	HandleSt' st st' m es -> (st -> m st') ->
	HandleSt' st st' m es'
expandHandleSt h o st reqs = maybe
	((Nothing ,) <$> o st)
	((((expand <$>) `first`) <$>) . h st) $ collapse reqs
	
mergeHandleSt :: (
	Monad m, Beforable es es',
	Mergeable (Occurred :$: es) (Occurred :$: es') (Occurred :$: (es :+: es'))
	) =>
	HandleSt' st st' m es -> (st -> m st') ->
	HandleSt' st' st'' m es' -> (st' -> m st'') ->
	HandleSt' st st'' m (es :+: es')
mergeHandleSt h1 o1 h2 o2 st reqs = do
	(r1, st') <- maybe ((Nothing ,) <$> o1 st) (h1 st) $ collapse reqs
	(r2, st'') <- maybe ((Nothing ,) <$> o2 st') (h2 st') $ collapse reqs
	pure (r1 `merge'` r2, st'')
