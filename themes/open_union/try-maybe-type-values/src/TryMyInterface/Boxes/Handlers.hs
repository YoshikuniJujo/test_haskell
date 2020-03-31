{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryMyInterface.Boxes.Handlers where

import MonadicFrp.MyInterface
import TryMyInterface.Boxes.Events
import Field

handleWithoutTime :: Field -> EvReqs GuiEv -> IO (EvOccs GuiEv)
handleWithoutTime f reqs = withNextEvent f \case
	ButtonEvent { ev_event_type = 4, ev_button = eb } | Just b <- button eb ->
		pure . expand $ OccurredMouseDown [b] >+ UnionListNil
	_ -> handleWithoutTime f reqs

button :: Button -> Maybe MouseBtn
button = \case
	1 -> Just MLeft; 2 -> Just MMiddle; 3 -> Just MRight
	4 -> Just MUp; 5 -> Just MDown; _ -> Nothing
