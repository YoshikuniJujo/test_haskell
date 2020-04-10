{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-orphans #-}

module MonadicFrp (
	-- * Types
	Sig, ISig, React, EvReqs, EvOccs, Request(..),
	Firstable, CollapsableOccurred,
	-- * Run
	interpret, interpretSig,
	-- * Handle
	Handle, Handle', retryHandle, mergeHandle,
	-- * React
	await, adjust, first,
	-- * Conversion
	emit, waitFor,
	-- * Transformation
	scanl, find,
	-- * Repetition
	repeat, spawn, parList,
	-- * Parallel composition
	at, until, indexBy,
	) where

import Prelude hiding (map, repeat, scanl, until)

import Data.Type.Set
import Data.UnionSet
import MonadicFrp.Sig
import MonadicFrp.React

type Handle m es = EvReqs es -> m (EvOccs es)
type Handle' m es = EvReqs es -> m (Maybe (EvOccs es))

retryHandle :: Monad m => Handle' m es -> Handle m es
retryHandle h reqs = h reqs >>= \case
	Just occs -> pure occs; Nothing -> retryHandle h reqs

infixr 6 `mergeHandle`

mergeHandle :: (
	Monad m,
	Expandable (Occurred :$: es) (Occurred :$: es :+: es'),
	Expandable (Occurred :$: es') (Occurred :$: es :+: es'),
	Collapsable (es :+: es') es, Collapsable (es :+: es') es',
	Mergeable (Occurred :$: es) (Occurred :$: es') (Occurred :$: es :+: es')
	) => Handle' m es -> Handle' m es' -> Handle' m (es :+: es')
mergeHandle h1 h2 reqs = do
	r1 <- maybe (pure Nothing) h1 $ collapse reqs
	r2 <- maybe (pure Nothing) h2 $ collapse reqs
	pure $ r1 `merge'` r2
