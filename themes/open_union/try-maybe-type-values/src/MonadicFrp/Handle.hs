{-# LANGUAGE TupleSections #-}
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
import Data.OneOrMore (Mergeable, Expandable, Collapsable, merge')

import qualified Data.OneOrMore as OOM

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
collapse hdl = maybe (pure Nothing) hdl . OOM.collapse

expand :: (Applicative m, Collapsable es' es, ExpandableOccurred es es') =>
	Handle' m es -> Handle' m es'
expand hdl = ((OOM.expand <$>) <$>) . collapse hdl

infixr 5 `before`

before :: (Monad m, Beforable es es') =>
	Handle' m es -> Handle' m es' -> Handle' m (es :+: es')
before hdl1 hdl2 rqs = maybe (expand hdl2 rqs) (pure . Just) =<< expand hdl1 rqs

infixr 6 `merge`

merge :: (
	Applicative m, Beforable es es', MergeableOccurred es es' (es :+: es')
	) => Handle' m es -> Handle' m es' -> Handle' m (es :+: es')
merge hdl1 hdl2 rqs = merge' <$> collapse hdl1 rqs <*> collapse hdl2 rqs

---------------------------------------------------------------------------
-- HANDLE WITH HANDLE
---------------------------------------------------------------------------

retrySt :: Monad m => HandleSt' st st m es -> HandleSt st m es
retrySt hdl st rqs = hdl st rqs >>= \(mocc, st') ->
	maybe (retrySt hdl st' rqs) (pure . (, st')) mocc

collapseSt :: (Applicative m, Collapsable es' es) =>
	(st -> EvReqs es -> m (Maybe (EvOccs es), st')) ->
	(st -> m st') -> st -> EvReqs es' -> m (Maybe (EvOccs es), st')
collapseSt hdl ot st = maybe ((Nothing ,) <$> ot st) (hdl st) . OOM.collapse

expandSt :: (Applicative m, Collapsable es' es, ExpandableOccurred es es') =>
	HandleSt' st st' m es -> (st -> m st') -> HandleSt' st st' m es'
expandSt hdl o st rqs = first (OOM.expand <$>) <$> collapseSt hdl o st rqs

beforeSt :: (Monad m, Beforable es es') =>
	HandleSt' st st' m es -> (st -> m st') ->
	HandleSt' st' st'' m es' -> (st' -> m st'') ->
	HandleSt' st st'' m (es :+: es')
beforeSt hdl1 ot1 hdl2 ot2 st rqs = expandSt hdl1 ot1 st rqs >>= \(mocc, st') ->
	maybe (expandSt hdl2 ot2 st' rqs) ((<$> ot2 st') . (,) . Just) mocc

mergeSt :: (Monad m, Beforable es es', MergeableOccurred es es' (es :+: es')) =>
	HandleSt' st st' m es -> (st -> m st') ->
	HandleSt' st' st'' m es' -> (st' -> m st'') ->
	HandleSt' st st'' m (es :+: es')
mergeSt hdl1 ot1 hdl2 ot2 st rqs = collapseSt hdl1 ot1 st rqs >>= \(mocc1, st') ->
	first (mocc1 `merge'`) <$> collapseSt hdl2 ot2 st' rqs
