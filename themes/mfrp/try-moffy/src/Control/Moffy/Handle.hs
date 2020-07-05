{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators, ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Handle (
	-- * Type
	Handle, Handle', HandleSt, HandleSt',
	ExpandableHandle, MergeableOccurred,
	-- * Composer
	retry, expand, before, merge, retrySt, expandSt, beforeSt, mergeSt
	) where

import Control.Arrow
import Data.Type.Set
import Data.OneOrMore hiding (merge, expand, collapse)

import qualified Data.OneOrMore as OOM

import Control.Moffy.Internal.React.Type

retry :: Monad m => Handle' m es -> Handle m es
retry hdl rqs = maybe (retry hdl rqs) pure =<< hdl rqs

collapse :: (Applicative m, Collapsable es' es) =>
	Handle' m es -> EvReqs es' -> m (Maybe (EvOccs es))
collapse hdl = maybe (pure Nothing) hdl . OOM.collapse

type ExpandableHandle es es' = (Collapsable es' es, ExpandableOccurred es es')

expand :: (Applicative m, ExpandableHandle es es') =>
	Handle' m es -> Handle' m es'
expand hdl = ((OOM.expand <$>) <$>) . collapse hdl

infixr 5 `before`

before :: (
	Monad m,
	ExpandableHandle es (es :+: es'), ExpandableHandle es' (es :+: es') ) =>
	Handle' m es -> Handle' m es' -> Handle' m (es :+: es')
before hdl1 hdl2 rqs = maybe (expand hdl2 rqs) (pure . Just) =<< expand hdl1 rqs

infixr 6 `merge`

merge :: (
	Applicative m,
	ExpandableHandle es (es :+: es'), ExpandableHandle es' (es :+: es'),
	MergeableOccurred es es' (es :+: es') ) =>
	Handle' m es -> Handle' m es' -> Handle' m (es :+: es')
merge hdl1 hdl2 rqs = merge' <$> collapse hdl1 rqs <*> collapse hdl2 rqs

retrySt :: Monad m => HandleSt' st st m es -> HandleSt st m es
retrySt hdl st rqs = hdl st rqs >>= \(mocc, st') ->
	maybe (retrySt hdl st' rqs) (pure . (, st')) mocc

collapseSt :: (Applicative m, Collapsable es' es) =>
	HandleSt' st st' m es -> (st -> m st') -> st -> EvReqs es' -> m (Maybe (EvOccs es), st')
collapseSt hdl ot st = maybe ((Nothing ,) <$> ot st) (hdl st) . OOM.collapse

expandSt :: (Applicative m, ExpandableHandle es es') =>
	HandleSt' st st' m es -> (st -> m st') -> HandleSt' st st' m es'
expandSt hdl o st rqs = first (OOM.expand <$>) <$> collapseSt hdl o st rqs


beforeSt :: (
	Monad m,
	ExpandableHandle es (es :+: es'), ExpandableHandle es' (es :+: es') ) =>
	HandleSt' st st' m es -> (st -> m st') ->
	HandleSt' st' st'' m es' -> (st' -> m st'') ->
	HandleSt' st st'' m (es :+: es')
beforeSt hdl1 ot1 hdl2 ot2 st rqs = expandSt hdl1 ot1 st rqs >>= \(mocc, st') ->
	maybe (expandSt hdl2 ot2 st' rqs) ((<$> ot2 st') . (,) . Just) mocc

mergeSt :: (
	Monad m,
	ExpandableHandle es (es :+: es'), ExpandableHandle es' (es :+: es'),
	MergeableOccurred es es' (es :+: es') ) =>
	HandleSt' st st' m es -> (st -> m st') ->
	HandleSt' st' st'' m es' -> (st' -> m st'') ->
	HandleSt' st st'' m (es :+: es')
mergeSt hdl1 ot1 hdl2 ot2 st rqs = collapseSt hdl1 ot1 st rqs >>= \(mocc1, st') ->
	first (mocc1 `merge'`) <$> collapseSt hdl2 ot2 st' rqs
