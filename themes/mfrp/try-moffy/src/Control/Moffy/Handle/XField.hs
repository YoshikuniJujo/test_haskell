{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Handle.XField (
	-- * Type
	GuiEv, CalcTextExtents,
	-- * Handle
	handle, handle', handleWith) where

import Control.Moffy (EvReqs, EvOccs)
import Control.Moffy.Event.Delete (DeleteEvent, pattern OccDeleteEvent)
import Control.Moffy.Event.Key (KeyEv)
import Control.Moffy.Event.Mouse (MouseEv)
import Control.Moffy.Event.CalcTextExtents
import Control.Moffy.Handle (Handle', ExpandableOccurred, before)
import Control.Moffy.Handle.XField.Key (pattern KeyEv)
import Control.Moffy.Handle.XField.Mouse (pattern MouseEv)
import Control.Moffy.Handle.XField.CalcTextExtents
import Data.Type.Set (Singleton, (:-), (:+:))
import Data.OneOrMore (pattern Singleton, expand)
import Data.Time (DiffTime)
import Field (
	Field, flushField, Event', evEvent, Event(..),
	withNextEvent, withNextEventTimeout', isDeleteEvent )

---------------------------------------------------------------------------

-- * GUI EV
-- * HANDLE

---------------------------------------------------------------------------
-- GUI EV
---------------------------------------------------------------------------

type GuiEv = DeleteEvent :- KeyEv :+: MouseEv

---------------------------------------------------------------------------
-- HANDLE
---------------------------------------------------------------------------

handle' :: Maybe DiffTime -> Field -> Handle' IO (CalcTextExtents :- GuiEv)
handle' mt f = (Just <$>) . handleCalcTextExtents f `before` handle mt f

handle :: Maybe DiffTime -> Field -> Handle' IO GuiEv
handle = handleWith \case
	KeyEv kev -> Just $ expand kev; MouseEv mev -> Just $ expand mev
	_ -> Nothing

handleWith :: ExpandableOccurred (Singleton DeleteEvent) es =>
	(Event' -> Maybe (EvOccs es)) ->
	Maybe DiffTime -> Field -> Handle' IO es
handleWith etoe Nothing f rqs = withNextEvent f $ eventToEv etoe f rqs
handleWith etoe (Just d) f rqs =
	withNextEventTimeout' f d $ maybe (pure Nothing) (eventToEv etoe f rqs)

eventToEv :: ( ExpandableOccurred (Singleton DeleteEvent) es ) =>
	(Event' -> Maybe (EvOccs es)) ->
	Field -> EvReqs es -> Event' -> IO (Maybe (EvOccs es))
eventToEv etoe f _rqs = \case
	(evEvent -> ev)
		| ExposeEvent {} <- ev -> Nothing <$ flushField f
		| isDeleteEvent f ev ->
			pure . Just . expand $ Singleton OccDeleteEvent
	ev -> pure $ etoe ev
