{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Handle.XField (
	-- * Type
	GuiEv,
	-- * Function
	handle, handleWith) where

import Data.Type.Set
import Data.OneOrMore
import Data.Time
import System.Exit

import Control.Moffy
import Control.Moffy.Handle hiding (expand)
import Control.Moffy.Event.Delete
import Control.Moffy.Event.Key
import Control.Moffy.Event.Mouse
import Control.Moffy.Handle.XField.Key
import Control.Moffy.Handle.XField.Mouse
import Field

type GuiEv = DeleteEvent :- KeyEv :+: MouseEv

handle :: Maybe DiffTime -> Field -> Handle' IO GuiEv
handle = handleWith \case
	KeyEv kev -> Just $ expand kev; MouseEv mev -> Just $ expand mev
	_ -> Nothing

handleWith :: (ExpandableOccurred (Singleton DeleteEvent) es) =>
	(Event' -> Maybe (EvOccs es)) -> Maybe DiffTime -> Field -> Handle' IO es
handleWith etoe Nothing f rqs = withNextEvent f $ eventToEv rqs etoe f
handleWith etoe (Just prd) f rqs = withNextEventTimeout' f
	(round $ prd * 1000000) $ maybe (pure Nothing) (eventToEv rqs etoe f)

eventToEv :: (
	Expandable (Singleton (Occurred DeleteEvent)) (Occurred :$: es) ) =>
	EvReqs es -> (Event' -> Maybe (EvOccs es)) -> Field -> Event' -> IO (Maybe (EvOccs es))
eventToEv _rqs etoe f = \case
	(evEvent -> ExposeEvent {}) -> Nothing <$ flushField f
	(evEvent -> DestroyWindowEvent {}) -> closeField f >> exitSuccess
	ev'@(evEvent -> ev)
		| isDeleteEvent f ev -> pure . Just . expand $ Singleton OccDeleteEvent
		| otherwise -> pure $ etoe ev'
