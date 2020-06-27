{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Moffy.Handle where

import Data.Type.Set
import Data.OneOrMore hiding (expand, collapse)

import qualified Data.OneOrMore as OOM

import Moffy.React

collapse :: (Applicative m, Collapsable es' es) =>
	Handle' m es -> EvReqs es' -> m (Maybe (EvOccs es))
collapse hdl = maybe (pure Nothing) hdl . OOM.collapse

expand :: (Applicative m, ExpandableOccurred es es', Collapsable es' es) =>
	Handle' m es -> Handle' m es'
expand hdl = ((OOM.expand <$>) <$>) . collapse hdl

infixr 5 `before`

before :: (
	Monad m,
	ExpandableOccurred es (es :+: es'), ExpandableOccurred es' (es :+: es'),
	Collapsable (es :+: es') es, Collapsable (es :+: es') es' ) =>
	Handle' m es -> Handle' m es' -> Handle' m (es :+: es')
before hdl1 hdl2 rqs = maybe (expand hdl2 rqs) (pure . Just) =<< expand hdl1 rqs
