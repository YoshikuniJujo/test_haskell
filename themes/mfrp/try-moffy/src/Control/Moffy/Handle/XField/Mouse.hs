{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Handle.XField.Mouse (
	-- * Pattern
	pattern MouseEv ) where

import Control.Moffy (EvOccs)
import Control.Moffy.Event.Mouse (
	MouseEv, MouseBtn(..),
	MouseDown, pattern OccMouseDown, MouseUp, pattern OccMouseUp,
	MouseMove, pattern OccMouseMove )
import Data.Type.Set (Singleton, (:-))
import Data.OneOrMore (pattern Singleton, (>-), expand)
import Field (Event', evEvent, Event(..))

---------------------------------------------------------------------------

pattern MouseEv :: EvOccs MouseEv -> Event'
pattern MouseEv mev <- (mouseEv -> Just mev)

type MoveAnd be = be :- Singleton MouseMove

mouseEv :: Event' -> Maybe (EvOccs MouseEv)
mouseEv ev = case (evEvent ev) of
	ButtonEvent { ev_event_type = 4, ev_button = eb, ev_x = x, ev_y = y }
		| Just b <- btn eb -> Just . expand $ down x y b
	ButtonEvent { ev_event_type = 5, ev_button = eb, ev_x = x, ev_y = y }
		| Just b <- btn eb -> Just . expand $ up x y b
	MotionEvent { ev_x = x, ev_y = y } -> Just . expand $ move x y
	_ -> Nothing
	where
	btn = \case
		1 -> Just MLeft; 2 -> Just MMiddle; 3 -> Just MRight
		4 -> Just MUp; 5 -> Just MDown; _ -> Nothing
	down x y b = OccMouseDown [b] >- move x y :: EvOccs (MoveAnd MouseDown)
	up x y b = OccMouseUp [b] >- move x y :: EvOccs (MoveAnd MouseUp)
	move x y = Singleton $ OccMouseMove (fromIntegral x, fromIntegral y)
