{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Handle.XField (handleWith) where

import Data.Type.Set
import Data.OneOrMore
import Data.Time
import System.Exit

import Control.Moffy
import Control.Moffy.Handle hiding (expand)
import Control.Moffy.Event.Delete
import Field

handleWith :: (
	Expandable (Singleton (Occurred DeleteEvent)) (Occurred :$: evs)
	) =>
	(Event' -> Maybe (EvOccs evs)) -> Maybe DiffTime -> Field -> Handle' IO evs
handleWith etoe Nothing f rqs = withNextEvent f $ eventToEv rqs etoe f
handleWith etoe (Just prd) f rqs = withNextEventTimeout' f
	(round $ prd * 1000000) $ maybe (pure Nothing) (eventToEv rqs etoe f)

eventToEv :: (
	Expandable (Singleton (Occurred DeleteEvent)) (Occurred :$: evs) ) =>
	EvReqs evs -> (Event' -> Maybe (EvOccs evs)) -> Field -> Event' -> IO (Maybe (EvOccs evs))
eventToEv _rqs etoe f = \case
	(ExposeEvent {}, _) -> Nothing <$ flushField f
	(DestroyWindowEvent {}, _) -> closeField f >> exitSuccess
	ev'@(ev, _)
		| isDeleteEvent f ev -> pure . Just . expand $ Singleton OccDeleteEvent
		| otherwise -> pure $ etoe ev'
