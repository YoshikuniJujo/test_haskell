{-# LANGUAGE LambdaCase, TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeOperators, ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Handle (
	-- * Constraint
	ExpandableHandle, ExpandableOccurred, MergeableOccurred,
	-- * Plain Handle
	-- ** Type
	Handle, Handle',
	-- ** Composer
	retry, sleep, sleepIfNothing, expand, before, merge,
	-- * Handle with State
	-- ** Type
	HandleSt, HandleSt', St, liftHandle, liftHandle', liftSt,
	-- ** Composer
	retrySt, expandSt, beforeSt, mergeSt,
	-- * Handle with Input and Output
	-- ** Type
	HandleIo', pushInput, popInput,
	-- ** Composer
	expandIo, beforeIo, mergeIo ) where

import Control.Arrow (first)
import Control.Moffy.Internal.React.Type (
	EvReqs, EvOccs, ExpandableOccurred, MergeableOccurred,
	Handle, HandleSt, St, liftHandle, liftSt )
import Control.Concurrent (threadDelay)
import Data.Type.Set ((:+:))
import Data.OneOrMore (Collapsable)
import Data.OneOrMoreApp (merge')

import qualified Data.OneOrMore as OOM (collapse)
import qualified Data.OneOrMoreApp as OOM (expand)

---------------------------------------------------------------------------

-- * CONSTRAINT
-- * PLAIN HANDLE
--	+ TYPE
--	+ COMPOSER
-- * HANDLE WITH STATE
--	+ TYPE
--	+ COMPOSER
-- * HANDLE WITH INPUT AND OUTPUT
--	+ TYPE
--	+ COMPOSER

---------------------------------------------------------------------------
-- CONSTRAINT
---------------------------------------------------------------------------

type ExpandableHandle es es' = (ExpandableOccurred es es', Collapsable es' es)

---------------------------------------------------------------------------
-- PLAIN HANDLE
---------------------------------------------------------------------------

-- TYPE

type Handle' m es = EvReqs es -> m (Maybe (EvOccs es))

-- COMPOSER

retry :: Monad m => Handle' m es -> Handle m es
retry hdl rqs = retry hdl rqs `maybe` pure =<< hdl rqs

sleep :: Int -> Handle' IO es -> Handle' IO es
sleep n hdl rqs = hdl rqs <* threadDelay n

sleepIfNothing :: Int -> Handle' IO es -> Handle' IO es
sleepIfNothing n hdl rqs = hdl rqs >>= \case
	Nothing -> Nothing <$ threadDelay n
	Just x -> pure $ Just x

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
((expand -> l) `before` (expand -> r)) rqs =
	r rqs `maybe` (pure . Just) =<< l rqs

infixr 6 `merge`, `mergeSt`

merge :: (
	Applicative m,
	ExpandableHandle es (es :+: es'), ExpandableHandle es' (es :+: es'),
	MergeableOccurred es es' (es :+: es') ) =>
	Handle' m es -> Handle' m es' -> Handle' m (es :+: es')
((collapse -> l) `merge` (collapse -> r)) rqs = merge' <$> l rqs <*> r rqs

---------------------------------------------------------------------------
-- HANDLE WITH STATE
---------------------------------------------------------------------------

-- TYPE

type HandleSt' st m es = EvReqs es -> St st m (Maybe (EvOccs es))
-- ^ > type HandleSt' st m es = HandleIo' st st m es

liftHandle' :: Functor m => Handle' m es -> HandleSt' st m es
liftHandle' = (liftSt .)

-- COMPOSER

retrySt :: Monad m => HandleSt' st m es -> HandleSt st m es
retrySt hdl rqs st = hdl rqs st >>= \(mo, st') ->
	(retrySt hdl rqs st' `maybe` (pure . (, st'))) mo

expandSt :: (Applicative m, ExpandableHandle es es') =>
	HandleSt' st m es -> HandleSt' st m es'
expandSt = (`expandIo` pure)

beforeSt :: (
	Monad m,
	ExpandableHandle es (es :+: es'), ExpandableHandle es' (es :+: es') ) =>
	HandleSt' st m es -> HandleSt' st m es' -> HandleSt' st m (es :+: es')
l `beforeSt` r = beforeIo l pure r pure

mergeSt :: (
	Monad m,
	ExpandableHandle es (es :+: es'), ExpandableHandle es' (es :+: es'),
	MergeableOccurred es es' (es :+: es') ) =>
	HandleSt' st m es -> HandleSt' st m es' -> HandleSt' st m (es :+: es')
l `mergeSt` r = mergeIo l pure r pure

---------------------------------------------------------------------------
-- HANDLE WITH INPUT AND OUTPUT
---------------------------------------------------------------------------

-- TYPE

type HandleIo' i o m es = EvReqs es -> i -> m (Maybe (EvOccs es), o)

pushInput :: (a -> HandleSt' st m es) -> HandleIo' (a, st) st m es
pushInput = (uncurry .) . flip

popInput :: HandleIo' (a, st) st m es -> a -> HandleSt' st m es
popInput = flip . (curry .)

-- COMPOSER

collapseIo :: (Applicative m, Collapsable es' es) =>
	HandleIo' i o m es -> (i -> m o) ->
	EvReqs es' -> i -> m (Maybe (EvOccs es), o)
collapseIo hdl nh = ((((Nothing ,) <$>) . nh) `maybe` hdl) . OOM.collapse

expandIo :: (Applicative m, ExpandableHandle es es') =>
	HandleIo' i o m es -> (i -> m o) -> HandleIo' i o m es'
expandIo hdl nh rqs = (first (OOM.expand <$>) <$>) . collapseIo hdl nh rqs

beforeIo :: (
	Monad m,
	ExpandableHandle es (es :+: es'), ExpandableHandle es' (es :+: es') ) =>
	HandleIo' i x m es -> (i -> m x) ->
	HandleIo' x o m es' -> (x -> m o) -> HandleIo' i o m (es :+: es')
beforeIo l nhl r nhr rqs st = expandIo l nhl rqs st >>= \(mo, st') ->
	(expandIo r nhr rqs st' `maybe` ((<$> nhr st') . (,) . Just)) mo

mergeIo :: (
	Monad m,
	ExpandableHandle es (es :+: es'), ExpandableHandle es' (es :+: es'),
	MergeableOccurred es es' (es :+: es') ) =>
	HandleIo' i x m es -> (i -> m x) ->
	HandleIo' x o m es' -> (x -> m o) -> HandleIo' i o m (es :+: es')
mergeIo l nhl r nhr rqs st = collapseIo l nhl rqs st >>= \(mo, st') ->
	first (mo `merge'`) <$> collapseIo r nhr rqs st'
