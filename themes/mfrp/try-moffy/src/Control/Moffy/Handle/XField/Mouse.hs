{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Handle.XField.Mouse (pattern MouseEv) where

import Foreign.C.Types
import Data.Type.Set
import Data.OneOrMore

import Control.Moffy
import Control.Moffy.Event.Mouse
import Field

pattern MouseEv :: EvOccs MouseEv -> Event'
pattern MouseEv ev <- (mouseEv -> Just ev)

mouseEv :: Event' -> Maybe (EvOccs MouseEv)
mouseEv = \case
	(ButtonEvent { ev_event_type = 4, ev_button = eb, ev_x = x, ev_y = y }, _)
		| Just b <- btn eb -> Just . expand $ omd x y [b]
	(ButtonEvent { ev_event_type = 5, ev_button = eb, ev_x = x, ev_y = y }, _)
		| Just b <- btn eb -> Just . expand $ omu x y [b]
	(MotionEvent { ev_x = x, ev_y = y }, _) -> Just . expand $ omm x y
	_ -> Nothing
	where
	btn = \case
		1 -> Just MLeft; 2 -> Just MMiddle; 3 -> Just MRight
		4 -> Just MUp; 5 -> Just MDown; _ -> Nothing
	omd :: CInt -> CInt -> [MouseBtn] -> EvOccs (MouseMove :- MouseDown :- 'Nil)
	omd x y bs = OccMouseDown bs >- omm x y
	omu :: CInt -> CInt -> [MouseBtn] -> EvOccs (MouseMove :- MouseUp :- 'Nil)
	omu x y bs = OccMouseUp bs >- omm x y
	omm :: CInt -> CInt -> EvOccs (Singleton MouseMove)
	omm x y = Singleton $ OccMouseMove (fromIntegral x, fromIntegral y)
