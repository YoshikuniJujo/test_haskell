{-# LANGUAGE LambdaCase, TupleSections #-}
{-# LANGUAGE DataKinds, TypeOperators, ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-orphans #-}

module MonadicFrp.Handle (
	-- * Types
	Handle, Handle', HandleSt, HandleSt',
	Beforable, ExpandableOccurred, MergeableOccurred,
	-- * Composer
	retry, expand, before, merge, retrySt, expandSt, beforeSt, mergeSt
	) where

import Control.Arrow (first)
import Data.Type.Set ((:+:), (:$:))
import Data.UnionSet (Mergeable, Expandable, Collapsable, merge')

import qualified Data.UnionSet as US

import MonadicFrp.React (
	Occurred, Handle, Handle', HandleSt, HandleSt', EvReqs, EvOccs )

---------------------------------------------------------------------------

-- * CONSTRAINT SYNONYM
-- * HANDLE WITH NO STATE
-- * HANDLE WITH STATE

---------------------------------------------------------------------------
-- CONSTRAINT SYNONYM
---------------------------------------------------------------------------

type Beforable es es' = (
	Collapsable (es :+: es') es, Collapsable (es :+: es') es',
	ExpandableOccurred es (es :+: es'),
	ExpandableOccurred es' (es :+: es') )

type MergeableOccurred es es' eses' =
	Mergeable (Occurred :$: es) (Occurred :$: es') (Occurred :$: eses')

type ExpandableOccurred es es' = Expandable (Occurred :$: es) (Occurred :$: es')

---------------------------------------------------------------------------
-- HANDLE WITH NO STATE
---------------------------------------------------------------------------

retry :: Monad m => Handle' m es -> Handle m es
retry hdl rqs = maybe (retry hdl rqs) pure =<< hdl rqs

collapse :: (Applicative m, Collapsable es' es) =>
	(EvReqs es -> m (Maybe (EvOccs es))) ->
	EvReqs es' -> m (Maybe (EvOccs es))
collapse hdl = maybe (pure Nothing) hdl . US.collapse

expand :: (Applicative m, Collapsable es' es, ExpandableOccurred es es') =>
	Handle' m es -> Handle' m es'
expand hdl = ((US.expand <$>) <$>) . collapse hdl

infixr 5 `before`

before :: (Monad m, Beforable es es') =>
	Handle' m es -> Handle' m es' -> Handle' m (es :+: es')
before hdl1 hdl2 rqs = maybe (expand hdl2 rqs) (pure . Just) =<< expand hdl1 rqs

infixr 6 `merge`

merge :: (
	Applicative m, Beforable es es', MergeableOccurred es es' (es :+: es')
	) => Handle' m es -> Handle' m es' -> Handle' m (es :+: es')
merge hdl1 hdl2 reqs = merge' <$> collapse hdl1 reqs <*> collapse hdl2 reqs

---------------------------------------------------------------------------
-- HANDLE WITH HANDLE
---------------------------------------------------------------------------

retrySt :: Monad m => HandleSt' st st m es -> HandleSt st m es
retrySt h st reqs = h st reqs >>= \case
	(Just occs, st') -> pure (occs, st')
	(Nothing, st') -> retrySt h st' reqs

expandSt :: (Monad m, Collapsable es' es, ExpandableOccurred es es') =>
	HandleSt' st st' m es -> (st -> m st') ->
	HandleSt' st st' m es'
expandSt h o st reqs = maybe
	((Nothing ,) <$> o st)
	((((US.expand <$>) `first`) <$>) . h st) $ US.collapse reqs

infixr 5 `beforeSt`

beforeSt :: (Monad m, Beforable es es') =>
	HandleSt' st st' m es -> (st -> m st') ->
	HandleSt' st' st'' m es' -> (st' -> m st'') ->
	HandleSt' st st'' m (es :+: es')
beforeSt h1 o1 h2 o2 st reqs = do
	(r, st') <- maybe ((Nothing ,) <$> o1 st) (h1 st) (US.collapse reqs)
	case r of
		Just occs -> (Just (US.expand occs) ,) <$> o2 st'
		Nothing -> first (US.expand <$>) <$> maybe ((Nothing ,) <$> o2 st') (h2 st') (US.collapse reqs)

infixr 6 `mergeSt`

mergeSt :: (Monad m, Beforable es es', MergeableOccurred es es' (es :+: es')) =>
	HandleSt' st st' m es -> (st -> m st') ->
	HandleSt' st' st'' m es' -> (st' -> m st'') ->
	HandleSt' st st'' m (es :+: es')
mergeSt h1 o1 h2 o2 st reqs = do
	(r1, st') <- maybe ((Nothing ,) <$> o1 st) (h1 st) $ US.collapse reqs
	(r2, st'') <- maybe ((Nothing ,) <$> o2 st') (h2 st') $ US.collapse reqs
	pure (r1 `merge'` r2, st'')
