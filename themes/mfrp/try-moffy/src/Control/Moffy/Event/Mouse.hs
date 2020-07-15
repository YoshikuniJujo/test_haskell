{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Event.Mouse (
	-- * Type
	-- ** Mouse Ev
	MouseEv,
	-- ** Basic
	MouseBtn(..), Point,
	-- ** Each Event
	MouseDown, pattern OccMouseDown,
	MouseUp, pattern OccMouseUp,
	MouseMove, pattern OccMouseMove,
	DeleteEvent, pattern OccDeleteEvent,

	-- * Event
	-- ** Mouse Down
	mouseDown, leftClick, middleClick, rightClick,
	-- ** Mouse Up
	mouseUp, leftUp, middleUp, rightUp,
	-- ** Mouse Move
	mouseMove,
	-- ** Delete Event
	deleteEvent ) where

import Control.Moffy (React, Request(..), await)
import Data.Type.Set (numbered, Set(Nil), Singleton, (:-))
import Data.Bool (bool)

---------------------------------------------------------------------------

-- * MOUSE DOWN
-- * MOUSE UP
-- * MOUSE MOVE
-- * DELETE EVENT
-- * MOUSE EV

---------------------------------------------------------------------------
-- MOUSE DOWN
---------------------------------------------------------------------------

data MouseDown = MouseDownReq deriving (Show, Eq, Ord)
data MouseBtn = MLeft | MMiddle | MRight | MUp | MDown deriving (Show, Eq, Ord)
numbered 9 [t| MouseDown |]
instance Request MouseDown where
	data Occurred MouseDown = OccMouseDown [MouseBtn]
		deriving (Show, Eq, Ord)

mouseDown :: React s (Singleton MouseDown) [MouseBtn]
mouseDown = await MouseDownReq \(OccMouseDown bs) -> bs

clickOn :: MouseBtn -> React s (Singleton MouseDown) ()
clickOn b = bool (clickOn b) (pure ()) . (b `elem`) =<< mouseDown

leftClick, middleClick, rightClick :: React s (Singleton MouseDown) ()
[leftClick, middleClick, rightClick] = clickOn <$> [MLeft, MMiddle, MRight]

---------------------------------------------------------------------------
-- MOUSE UP
---------------------------------------------------------------------------

data MouseUp = MouseUpReq deriving (Show, Eq, Ord)
numbered 9 [t| MouseUp |]
instance Request MouseUp where
	data Occurred MouseUp = OccMouseUp [MouseBtn] deriving (Show, Eq, Ord)

mouseUp :: React s (Singleton MouseUp) [MouseBtn]
mouseUp = await MouseUpReq \(OccMouseUp bs) -> bs

releaseOn :: MouseBtn -> React s (Singleton MouseUp) ()
releaseOn b = bool (releaseOn b) (pure ()) . (b `elem`) =<< mouseUp

leftUp, middleUp, rightUp :: React s (Singleton MouseUp) ()
[leftUp, middleUp, rightUp] = releaseOn <$> [MLeft, MMiddle, MRight]

---------------------------------------------------------------------------
-- MOUSE MOVE
---------------------------------------------------------------------------

data MouseMove = MouseMoveReq deriving (Show, Eq, Ord)
type Point = (Integer, Integer)
numbered 9 [t| MouseMove |]
instance Request MouseMove where
	data Occurred MouseMove = OccMouseMove Point deriving (Show, Eq, Ord)

mouseMove :: React s (Singleton MouseMove) Point
mouseMove  = await MouseMoveReq \(OccMouseMove p) -> p

---------------------------------------------------------------------------
-- DELETE EVENT
---------------------------------------------------------------------------

data DeleteEvent = DeleteEventReq deriving (Show, Eq, Ord)
numbered 9 [t| DeleteEvent |]
instance Request DeleteEvent where
	data Occurred DeleteEvent = OccDeleteEvent deriving (Show, Eq, Ord)

deleteEvent :: React s (Singleton DeleteEvent) ()
deleteEvent = await DeleteEventReq \OccDeleteEvent -> ()

---------------------------------------------------------------------------
-- MOUSE EV
---------------------------------------------------------------------------

type MouseEv = MouseDown :- MouseUp :- MouseMove :- DeleteEvent :- 'Nil
