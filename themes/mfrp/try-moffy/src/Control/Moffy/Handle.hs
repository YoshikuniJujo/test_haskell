{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeOperators, ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Handle (
	-- * Constraint
	ExpandableHandle, ExpandableOccurred, MergeableOccurred,
	-- * Composer
	-- ** Plain
	Handle, Handle', retry, expand, before, merge,
	-- ** With State
	HandleSt, HandleIo', St, liftSt, retrySt, expandSt, beforeSt, beforeSt', mergeSt, mergeSt' ) where

import Control.Arrow (first)
import Control.Moffy.Internal.React.Type (
	Handle, HandleSt, St, liftSt, EvReqs, EvOccs, Occurred )
import Data.Type.Set ((:+:), (:$:))
import Data.OneOrMore (Expandable, Collapsable, Mergeable, merge')

import qualified Data.OneOrMore as OOM

---------------------------------------------------------------------------

-- * CONSTRAINT
-- * PLAIN
-- * WITH STATE

---------------------------------------------------------------------------
-- CONSTRAINT
---------------------------------------------------------------------------

type ExpandableHandle es es' = (Collapsable es' es, ExpandableOccurred es es')
type ExpandableOccurred es es' = Expandable (Occurred :$: es) (Occurred :$: es')
type MergeableOccurred es es' mrg =
	Mergeable (Occurred :$: es) (Occurred :$: es') (Occurred :$: mrg)

---------------------------------------------------------------------------
-- PLAIN
---------------------------------------------------------------------------

type Handle' m es = EvReqs es -> m (Maybe (EvOccs es))

retry :: Monad m => Handle' m es -> Handle m es
retry hdl rqs = maybe (retry hdl rqs) pure =<< hdl rqs

collapse :: (Applicative m, Collapsable es' es) =>
	Handle' m es -> EvReqs es' -> m (Maybe (EvOccs es))
collapse hdl = maybe (pure Nothing) hdl . OOM.collapse

expand :: (Applicative m, ExpandableHandle es es') =>
	Handle' m es -> Handle' m es'
expand hdl = ((OOM.expand <$>) <$>) . collapse hdl

infixr 5 `before`, `beforeSt'`

before :: (
	Monad m,
	ExpandableHandle es (es :+: es'), ExpandableHandle es' (es :+: es') ) =>
	Handle' m es -> Handle' m es' -> Handle' m (es :+: es')
before (expand -> l) (expand -> r) rqs = maybe (r rqs) (pure . Just) =<< l rqs

infixr 6 `merge`, `mergeSt'`

merge :: (
	Applicative m,
	ExpandableHandle es (es :+: es'), ExpandableHandle es' (es :+: es'),
	MergeableOccurred es es' (es :+: es') ) =>
	Handle' m es -> Handle' m es' -> Handle' m (es :+: es')
merge (collapse -> l) (collapse -> r) rqs = merge' <$> l rqs <*> r rqs

---------------------------------------------------------------------------
-- WITH STATE
---------------------------------------------------------------------------

type HandleIo' i o m es = EvReqs es -> i -> m (Maybe (EvOccs es), o)

retrySt :: Monad m => HandleIo' st st m es -> HandleSt st m es
retrySt hdl rqs st = hdl rqs st >>= \(mo, st') ->
	maybe (retrySt hdl rqs st') (pure . (, st')) mo

collapseSt :: (Applicative m, Collapsable es' es) =>
	HandleIo' st st' m es -> (st -> m st') ->
	EvReqs es' -> st -> m (Maybe (EvOccs es), st')
collapseSt hdl ot = maybe (((Nothing ,) <$>) . ot) hdl . OOM.collapse

expandSt :: (Applicative m, ExpandableHandle es es') =>
	HandleIo' st st' m es -> (st -> m st') -> HandleIo' st st' m es'
expandSt hdl ot st rqs = first (OOM.expand <$>) <$> collapseSt hdl ot st rqs

beforeSt :: (
	Monad m,
	ExpandableHandle es (es :+: es'), ExpandableHandle es' (es :+: es') ) =>
	HandleIo' st st' m es -> (st -> m st') ->
	HandleIo' st' st'' m es' -> (st' -> m st'') ->
	HandleIo' st st'' m (es :+: es')
beforeSt l otl r otr rqs st = expandSt l otl rqs st >>= \(mo, st') ->
	maybe (expandSt r otr rqs st') ((<$> otr st') . (,) . Just) mo

beforeSt' :: (
	Monad m,
	ExpandableHandle es (es :+: es'), ExpandableHandle es' (es :+: es') ) =>
	HandleIo' st st m es -> HandleIo' st st m es' -> HandleIo' st st m (es :+: es')
beforeSt' l r = beforeSt l pure r pure

mergeSt :: (
	Monad m,
	ExpandableHandle es (es :+: es'), ExpandableHandle es' (es :+: es'),
	MergeableOccurred es es' (es :+: es') ) =>
	HandleIo' st st' m es -> (st -> m st') ->
	HandleIo' st' st'' m es' -> (st' -> m st'') ->
	HandleIo' st st'' m (es :+: es')
mergeSt l otl r otr rqs st = collapseSt l otl rqs st >>= \(mo, st') ->
	first (mo `merge'`) <$> collapseSt r otr rqs st'

mergeSt' :: (
	Monad m, ExpandableHandle es (es :+: es'), ExpandableHandle es' (es :+: es'),
	MergeableOccurred es es' (es :+: es') ) =>
	HandleIo' st st m es -> HandleIo' st st m es' -> HandleIo' st st m (es :+: es')
mergeSt' h1 h2 = mergeSt h1 pure h2 pure
