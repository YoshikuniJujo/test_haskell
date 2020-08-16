{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Event.Mouse (
	-- * Type
	MouseEv, MouseBtn(..), Point,
	-- * Event
	-- ** Mouse Down
	MouseDown, pattern OccMouseDown,
	mouseDown, leftClick, middleClick, rightClick,
	-- ** Mouse Up
	MouseUp, pattern OccMouseUp, mouseUp, leftUp, middleUp, rightUp,
	-- ** Mouse Move
	MouseMove, pattern OccMouseMove, mouseMove
	) where

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
numbered [t| MouseDown |]
instance Request MouseDown where
	data Occurred MouseDown = OccMouseDown MouseBtn
		deriving (Show, Eq, Ord)

mouseDown :: React s (Singleton MouseDown) MouseBtn
mouseDown = await MouseDownReq \(OccMouseDown bs) -> bs

clickOn :: MouseBtn -> React s (Singleton MouseDown) ()
clickOn b = bool (clickOn b) (pure ()) . (== b) =<< mouseDown

leftClick, middleClick, rightClick :: React s (Singleton MouseDown) ()
[leftClick, middleClick, rightClick] = clickOn <$> [MLeft, MMiddle, MRight]

---------------------------------------------------------------------------
-- MOUSE UP
---------------------------------------------------------------------------

data MouseUp = MouseUpReq deriving (Show, Eq, Ord)
numbered [t| MouseUp |]
instance Request MouseUp where
	data Occurred MouseUp = OccMouseUp MouseBtn deriving (Show, Eq, Ord)

mouseUp :: React s (Singleton MouseUp) MouseBtn
mouseUp = await MouseUpReq \(OccMouseUp bs) -> bs

releaseOn :: MouseBtn -> React s (Singleton MouseUp) ()
releaseOn b = bool (releaseOn b) (pure ()) . (== b) =<< mouseUp

leftUp, middleUp, rightUp :: React s (Singleton MouseUp) ()
[leftUp, middleUp, rightUp] = releaseOn <$> [MLeft, MMiddle, MRight]

---------------------------------------------------------------------------
-- MOUSE MOVE
---------------------------------------------------------------------------

data MouseMove = MouseMoveReq deriving (Show, Eq, Ord)
type Point = (Integer, Integer)
numbered [t| MouseMove |]
instance Request MouseMove where
	data Occurred MouseMove = OccMouseMove Point deriving (Show, Eq, Ord)

mouseMove :: React s (Singleton MouseMove) Point
mouseMove  = await MouseMoveReq \(OccMouseMove p) -> p

---------------------------------------------------------------------------
-- MOUSE EV
---------------------------------------------------------------------------

type MouseEv = MouseDown :- MouseUp :- MouseMove :- 'Nil
