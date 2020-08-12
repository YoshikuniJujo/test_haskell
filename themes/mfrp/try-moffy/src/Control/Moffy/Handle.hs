{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeOperators, ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Handle (
	-- * Constraint
	ExpandableHandle, ExpandableOccurred, MergeableOccurred,
	-- * Handle and Function
	-- ** Plain
	-- *** Type
	Handle, Handle',
	-- *** Composer
	retry, expand, before, merge,
	-- ** With State
	-- *** Type
	HandleSt, HandleSt', liftHandle, liftHandle', St, liftSt,
	-- *** Composer
	retrySt, expandSt, beforeSt, mergeSt,
	-- ** With Input and Output
	-- *** Type
	HandleIo', pushInput, popInput,
	-- *** Composer
	expandIo, beforeIo, mergeIo ) where

import Control.Arrow (first)
import Control.Moffy.Internal.React.Type (
	Handle, HandleSt, liftHandle, St, liftSt, EvReqs, EvOccs, Occurred )
import Data.Type.Set ((:+:), (:$:))
import Data.OneOrMore (Expandable, Collapsable, Mergeable)

import qualified Data.OneOrMore as OOM (expand, collapse, merge')

---------------------------------------------------------------------------

-- * CONSTRAINT
-- * PLAIN
--	+ TYPE
--	+ COMPOSER
-- * WITH STATE
--	+ TYPE
--	+ COMPOSER
-- * WITH INPUT AND OUTPUT
--	+ TYPE
--	+ COMPOSER

---------------------------------------------------------------------------
-- CONSTRAINT
---------------------------------------------------------------------------

type ExpandableHandle es es' = (ExpandableOccurred es es', Collapsable es' es)
type ExpandableOccurred es es' = Expandable (Occurred :$: es) (Occurred :$: es')
type MergeableOccurred es es' mrg =
	Mergeable (Occurred :$: es) (Occurred :$: es') (Occurred :$: mrg)

---------------------------------------------------------------------------
-- PLAIN
---------------------------------------------------------------------------

-- TYPE

type Handle' m es = EvReqs es -> m (Maybe (EvOccs es))

-- COMPOSER

retry :: Monad m => Handle' m es -> Handle m es
retry hdl rqs = retry hdl rqs `maybe` pure =<< hdl rqs

collapse :: (Applicative m, Collapsable es' es) =>
	Handle' m es -> EvReqs es' -> m (Maybe (EvOccs es))
collapse hdl = (pure Nothing `maybe` hdl) . OOM.collapse

expand :: (Applicative m, ExpandableHandle es es') =>
	Handle' m es -> Handle' m es'
expand hdl = ((OOM.expand <$>) <$>) . collapse hdl

infixr 5 `before`, `beforeSt`

before :: (
	Monad m,
	ExpandableHandle es (es :+: es'), ExpandableHandle es' (es :+: es') ) =>
	Handle' m es -> Handle' m es' -> Handle' m (es :+: es')
before (expand -> l) (expand -> r) rqs = r rqs `maybe` (pure . Just) =<< l rqs

infixr 6 `merge`, `mergeSt`

merge :: (
	Applicative m,
	ExpandableHandle es (es :+: es'), ExpandableHandle es' (es :+: es'),
	MergeableOccurred es es' (es :+: es') ) =>
	Handle' m es -> Handle' m es' -> Handle' m (es :+: es')
merge (collapse -> l) (collapse -> r) rqs = OOM.merge' <$> l rqs <*> r rqs

---------------------------------------------------------------------------
-- WITH STATE
---------------------------------------------------------------------------

-- TYPE

type HandleSt' st m es = EvReqs es -> St st m (Maybe (EvOccs es))
-- ^ > type HandleSt' st m es = HandleIo' st st m es

liftHandle' :: Functor m => Handle' m es -> HandleSt' st m es
liftHandle' = (liftSt .)

-- COMPOSER

retrySt :: Monad m => HandleSt' st m es -> HandleSt st m es
retrySt hdl rqs st = hdl rqs st >>= \(mo, st') ->
	maybe (retrySt hdl rqs st') (pure . (, st')) mo

expandSt :: (Applicative m, ExpandableHandle es es') =>
	HandleSt' st m es -> HandleSt' st m es'
expandSt hdl = expandIo hdl pure

beforeSt :: (
	Monad m,
	ExpandableHandle es (es :+: es'), ExpandableHandle es' (es :+: es') ) =>
	HandleSt' st m es -> HandleSt' st m es' -> HandleSt' st m (es :+: es')
beforeSt l r = beforeIo l pure r pure

mergeSt :: (
	Monad m, ExpandableHandle es (es :+: es'), ExpandableHandle es' (es :+: es'),
	MergeableOccurred es es' (es :+: es') ) =>
	HandleSt' st m es -> HandleSt' st m es' -> HandleSt' st m (es :+: es')
mergeSt h1 h2 = mergeIo h1 pure h2 pure

---------------------------------------------------------------------------
-- WITH INPUT AND OUTPUT
---------------------------------------------------------------------------

-- TYPE

type HandleIo' i o m es = EvReqs es -> i -> m (Maybe (EvOccs es), o)

pushInput :: (a -> HandleSt' st m es) -> HandleIo' (a, st) st m es
pushInput hdl rqs (x, st) = hdl x rqs st

popInput :: HandleIo' (a, st) st m es -> a -> HandleSt' st m es
popInput hdl x rqs st = hdl rqs (x, st)

-- COMPOSER

collapseIo :: (Applicative m, Collapsable es' es) =>
	HandleIo' i o m es -> (i -> m o) ->
	EvReqs es' -> i -> m (Maybe (EvOccs es), o)
collapseIo hdl ot = maybe (((Nothing ,) <$>) . ot) hdl . OOM.collapse

expandIo :: (Applicative m, ExpandableHandle es es') =>
	HandleIo' i o m es -> (i -> m o) -> HandleIo' i o m es'
expandIo hdl ot st rqs = first (OOM.expand <$>) <$> collapseIo hdl ot st rqs

beforeIo :: (
	Monad m,
	ExpandableHandle es (es :+: es'), ExpandableHandle es' (es :+: es') ) =>
	HandleIo' i x m es -> (i -> m x) ->
	HandleIo' x o m es' -> (x -> m o) ->
	HandleIo' i o m (es :+: es')
beforeIo l otl r otr rqs st = expandIo l otl rqs st >>= \(mo, st') ->
	maybe (expandIo r otr rqs st') ((<$> otr st') . (,) . Just) mo

mergeIo :: (
	Monad m,
	ExpandableHandle es (es :+: es'), ExpandableHandle es' (es :+: es'),
	MergeableOccurred es es' (es :+: es') ) =>
	HandleIo' i x m es -> (i -> m x) ->
	HandleIo' x o m es' -> (x -> m o) ->
	HandleIo' i o m (es :+: es')
mergeIo l otl r otr rqs st = collapseIo l otl rqs st >>= \(mo, st') ->
	first (mo `OOM.merge'`) <$> collapseIo r otr rqs st'
