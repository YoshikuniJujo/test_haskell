{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Moffy.XFieldHandle.Mouse where

import Data.OneOrMore

import Moffy.React
import Moffy.Event.Mouse
import Field

handleMouse :: Field -> Handle' IO MouseEv -- (Singleton MouseDown)
handleMouse f _rqs = withNextEvent f $ eventToEv f

eventToEv :: Field -> Event -> IO (Maybe (EvOccs MouseEv)) -- IO (Maybe (EvOccs (Singleton MouseDown)))
eventToEv _f = \case
	ButtonEvent { ev_event_type = 4, ev_button = eb, ev_x = _x, ev_y = _y }
		| Just b <- btn eb -> pure . Just . expand . singleton $ OccMouseDown [b]
	_ -> pure Nothing
	where
	btn = \case
		1 -> Just MLeft; 2 -> Just MMiddle; 3 -> Just MRight
		4 -> Just MUp; 5 -> Just MDown; _ -> Nothing
