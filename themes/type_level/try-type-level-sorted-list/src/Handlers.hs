{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Handlers where

import Data.List.NonEmpty

import OpenUnionValue
import React
import Field

handleWithoutTime :: Field -> EvReqs GuiEv -> IO (EvOccs GuiEv)
handleWithoutTime f reqs = withNextEvent f \case
	ButtonEvent { ev_event_type = 4, ev_button = b } ->
		pure $ inj (OccurredMouseDown [button b]) :| []
	ButtonEvent { ev_event_type = 5, ev_button = b } ->
		pure $ inj (OccurredMouseUp [button b]) :| []
	_ -> handleWithoutTime f reqs

button :: Button -> MouseBtn
button = \case
	1 -> MLeft; 2 -> MMiddle; 3 -> MRight; 4 -> MUp; 5 -> MDown
	_ -> error "Unknown button"
