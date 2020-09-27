{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE TypeOperators #-}
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
import Field (Event', evEvent, Event(..))

import Data.OneOrMoreApp

import Control.Moffy.Event.Window

---------------------------------------------------------------------------

pattern MouseEv :: EvOccs MouseEv -> Event'
pattern MouseEv mev <- (mouseEv -> Just mev)

type MoveAnd e = e :- Singleton MouseMove

mouseEv :: Event' -> Maybe (EvOccs MouseEv)
mouseEv = (. evEvent) \case
	ButtonEvent { ev_event_type = 4, ev_button = eb, ev_x = x, ev_y = y }
		| b <- btn eb -> Just . expand $ down x y b
	ButtonEvent { ev_event_type = 5, ev_button = eb, ev_x = x, ev_y = y }
		| b <- btn eb -> Just . expand $ up x y b
	MotionEvent { ev_x = x, ev_y = y } -> Just . expand $ move x y
	_ -> Nothing
	where
	down x y b = OccMouseDown (WindowId 0) b >- move x y :: EvOccs (MoveAnd MouseDown)
	up x y b = OccMouseUp (WindowId 0) b >- move x y :: EvOccs (MoveAnd MouseUp)
	move x y = Singleton $ OccMouseMove (WindowId 0) (fromIntegral x, fromIntegral y)
	btn = \case
		1 -> ButtonLeft; 2 -> ButtonMiddle; 3 -> ButtonRight
		4 -> ScrollUp; 5 -> ScrollDown; 6 -> ScrollLeft; 7 -> ScrollRight
		8 -> PageBack; 9 -> PageForward
		10 -> ButtonFn1; 11 -> ButtonFn2; 12 -> ButtonFn3
		n -> ButtonUnknown n
