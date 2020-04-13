{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-orphans #-}

module MonadicFrp.Handle (
	-- * Types
	Sig, React, Handle, Handle', EvReqs, EvOccs,
	-- * Composer
	retry, before, merge
	) where

import Prelude hiding (map, repeat, scanl, until)

import Data.Type.Set
import Data.UnionSet hiding (merge)
import MonadicFrp.Sig
import MonadicFrp.React

retry :: Monad m => Handle' m es -> Handle m es
retry h reqs = h reqs >>= \case
	Just occs -> pure occs; Nothing -> retry h reqs

infixr 5 `before`

before :: (
	Monad m,
	Expandable (Occurred :$: es) (Occurred :$: es :+: es'),
	Expandable (Occurred :$: es') (Occurred :$: es :+: es'),
	Collapsable (es :+: es') es, Collapsable (es :+: es') es'
	) => Handle' m es -> Handle' m es' -> Handle' m (es :+: es')
before h1 h2 reqs = maybe (pure Nothing) h1 (collapse reqs) >>= \case
	Just occs -> pure . Just $ expand occs
	Nothing -> (expand <$>) <$> maybe (pure Nothing) h2 (collapse reqs)

infixr 6 `merge`

merge :: (
	Monad m,
	Expandable (Occurred :$: es) (Occurred :$: es :+: es'),
	Expandable (Occurred :$: es') (Occurred :$: es :+: es'),
	Collapsable (es :+: es') es, Collapsable (es :+: es') es',
	Mergeable (Occurred :$: es) (Occurred :$: es') (Occurred :$: es :+: es')
	) => Handle' m es -> Handle' m es' -> Handle' m (es :+: es')
merge h1 h2 reqs = do
	r1 <- maybe (pure Nothing) h1 $ collapse reqs
	r2 <- maybe (pure Nothing) h2 $ collapse reqs
	pure $ r1 `merge'` r2
