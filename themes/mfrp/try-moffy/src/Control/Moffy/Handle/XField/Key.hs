{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Handle.XField.Key (
	-- * Pattern
	pattern KeyEv ) where

import Control.Moffy (EvOccs)
import Control.Moffy.Event.Window
import Control.Moffy.Event.Key (
	KeyEv, Key(..), pattern OccKeyDown, pattern OccKeyUp )
import Field (Event', Event(..), evEvent, evKeySym)

import Data.OneOrMoreApp

---------------------------------------------------------------------------

pattern KeyEv :: EvOccs KeyEv -> Event'
pattern KeyEv kev <- (keyEv -> Just kev)

keyEv :: Event' -> Maybe (EvOccs KeyEv)
keyEv ev = case (evEvent ev, evKeySym ev) of
	(KeyEvent { ev_event_type = 2 }, Just s) ->
		Just . expand . Singleton . OccKeyDown (WindowId 0) $ Key s
	(KeyEvent { ev_event_type = 3 }, Just s) ->
		Just . expand . Singleton . OccKeyUp (WindowId 0) $ Key s
	_ -> Nothing
