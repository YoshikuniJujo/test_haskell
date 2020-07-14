{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeOperators, ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Handle (
	-- * Constraint
	ExpandableHandle, MergeableOccurred,
	-- * Composer
	-- ** Plain
	Handle, Handle', retry, expand, before, merge,
	-- ** With State
	HandleSt, HandleSt', retrySt, expandSt, beforeSt, mergeSt
	) where

import Control.Arrow (first)
import Control.Moffy.Internal.React.Type (
	Handle, HandleSt, EvReqs, EvOccs, Occurred )
import Data.Type.Set ((:+:), (:$:))
import Data.OneOrMore (Expandable, Collapsable, Mergeable, merge')

import qualified Data.OneOrMore as OOM

---------------------------------------------------------------------------

-- * PLAIN
-- * WITH STATE

---------------------------------------------------------------------------
-- PLAIN
---------------------------------------------------------------------------

type Handle' m es = EvReqs es -> m (Maybe (EvOccs es))

retry :: Monad m => Handle' m es -> Handle m es
retry hdl rqs = maybe (retry hdl rqs) pure =<< hdl rqs

collapse :: (Applicative m, Collapsable es' es) =>
	Handle' m es -> EvReqs es' -> m (Maybe (EvOccs es))
collapse hdl = maybe (pure Nothing) hdl . OOM.collapse

type ExpandableHandle es es' =
	(Collapsable es' es, Expandable (Occurred :$: es) (Occurred :$: es'))

expand :: (Applicative m, ExpandableHandle es es') =>
	Handle' m es -> Handle' m es'
expand hdl = ((OOM.expand <$>) <$>) . collapse hdl

infixr 5 `before`

before :: (
	Monad m,
	ExpandableHandle es (es :+: es'), ExpandableHandle es' (es :+: es') ) =>
	Handle' m es -> Handle' m es' -> Handle' m (es :+: es')
before (expand -> l) (expand -> r) rqs = maybe (l rqs) (pure . Just) =<< r rqs

type MergeableOccurred es es' mrg =
	Mergeable (Occurred :$: es) (Occurred :$: es') (Occurred :$: mrg)

infixr 6 `merge`

merge :: (
	Applicative m,
	ExpandableHandle es (es :+: es'), ExpandableHandle es' (es :+: es'),
	MergeableOccurred es es' (es :+: es') ) =>
	Handle' m es -> Handle' m es' -> Handle' m (es :+: es')
merge (collapse -> l) (collapse -> r) rqs = merge' <$> l rqs <*> r rqs

---------------------------------------------------------------------------
-- WITH STATE
---------------------------------------------------------------------------

type HandleSt' st st' m es = EvReqs es -> st -> m (Maybe (EvOccs es), st')

retrySt :: Monad m => HandleSt' st st m es -> HandleSt st m es
retrySt hdl rqs st = hdl rqs st >>= \(mocc, st') ->
	maybe (retrySt hdl rqs st') (pure . (, st')) mocc

collapseSt :: (Applicative m, Collapsable es' es) =>
	HandleSt' st st' m es -> (st -> m st') -> EvReqs es' -> st -> m (Maybe (EvOccs es), st')
collapseSt hdl ot rqs = maybe (((Nothing ,) <$>) . ot) hdl (OOM.collapse rqs)

expandSt :: (Applicative m, ExpandableHandle es es') =>
	HandleSt' st st' m es -> (st -> m st') -> HandleSt' st st' m es'
expandSt hdl o st rqs = first (OOM.expand <$>) <$> collapseSt hdl o st rqs


beforeSt :: (
	Monad m,
	ExpandableHandle es (es :+: es'), ExpandableHandle es' (es :+: es') ) =>
	HandleSt' st st' m es -> (st -> m st') ->
	HandleSt' st' st'' m es' -> (st' -> m st'') ->
	HandleSt' st st'' m (es :+: es')
beforeSt hdl1 ot1 hdl2 ot2 rqs st = expandSt hdl1 ot1 rqs st >>= \(mocc, st') ->
	maybe (expandSt hdl2 ot2 rqs st') ((<$> ot2 st') . (,) . Just) mocc

mergeSt :: (
	Monad m,
	ExpandableHandle es (es :+: es'), ExpandableHandle es' (es :+: es'),
	MergeableOccurred es es' (es :+: es') ) =>
	HandleSt' st st' m es -> (st -> m st') ->
	HandleSt' st' st'' m es' -> (st' -> m st'') ->
	HandleSt' st st'' m (es :+: es')
mergeSt hdl1 ot1 hdl2 ot2 rqs st = collapseSt hdl1 ot1 rqs st >>= \(mocc1, st') ->
	first (mocc1 `merge'`) <$> collapseSt hdl2 ot2 rqs st'
