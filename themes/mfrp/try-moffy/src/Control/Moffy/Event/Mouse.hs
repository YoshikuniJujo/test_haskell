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
	MouseMove, pattern OccMouseMove, mouseMove, mousePos,
	-- ** Mouse Scroll
	MouseScroll, pattern OccMouseScroll, mouseScroll
	) where

import Prelude hiding (repeat)

import Control.Moffy (Sig, React, Request(..), await, repeat)
import Data.Type.Set (numbered, pattern Nil, Singleton, (:-))
import Data.Bool (bool)
import Data.Word (Word32)

import Control.Moffy.Event.Window

---------------------------------------------------------------------------

-- * MOUSE DOWN
-- * MOUSE UP
-- * MOUSE MOVE
-- * MOUSE EV

---------------------------------------------------------------------------
-- MOUSE DOWN
---------------------------------------------------------------------------

data MouseDown = MouseDownReq deriving (Show, Eq, Ord)
numbered [t| MouseDown |]
instance Request MouseDown where
	data Occurred MouseDown = OccMouseDown WindowId MouseBtn deriving Show

data MouseBtn
	= ButtonLeft | ButtonMiddle | ButtonRight
	| ScrollUp | ScrollDown | ScrollLeft | ScrollRight
	| PageBack | PageForward | ButtonFn1 | ButtonFn2 | ButtonFn3
	| ButtonUnknown Word32
	deriving (Show, Eq, Ord)

mouseDown :: WindowId -> React s (Singleton MouseDown) MouseBtn
mouseDown wid0 = maybe (mouseDown wid0) pure =<<
	await MouseDownReq \(OccMouseDown wid b) -> bool Nothing (Just b) (wid == wid0)

clickOn :: WindowId -> MouseBtn -> React s (Singleton MouseDown) ()
clickOn wid b = bool (clickOn wid b) (pure ()) . (== b) =<< mouseDown wid

leftClick, middleClick, rightClick :: WindowId -> React s (Singleton MouseDown) ()
[leftClick, middleClick, rightClick] =
	flip clickOn <$> [ButtonLeft, ButtonMiddle, ButtonRight]

---------------------------------------------------------------------------
-- MOUSE UP
---------------------------------------------------------------------------

data MouseUp = MouseUpReq deriving (Show, Eq, Ord)
numbered [t| MouseUp |]
instance Request MouseUp where
	data Occurred MouseUp = OccMouseUp MouseBtn deriving Show

mouseUp :: React s (Singleton MouseUp) MouseBtn
mouseUp = await MouseUpReq \(OccMouseUp b) -> b

releaseOn :: MouseBtn -> React s (Singleton MouseUp) ()
releaseOn b = bool (releaseOn b) (pure ()) . (== b) =<< mouseUp

leftUp, middleUp, rightUp :: React s (Singleton MouseUp) ()
[leftUp, middleUp, rightUp] =
	releaseOn <$> [ButtonLeft, ButtonMiddle, ButtonRight]

---------------------------------------------------------------------------
-- MOUSE MOVE
---------------------------------------------------------------------------

data MouseMove = MouseMoveReq deriving (Show, Eq, Ord)
numbered [t| MouseMove |]
instance Request MouseMove where
	data Occurred MouseMove = OccMouseMove Point deriving Show

type Point = (Double, Double)

mouseMove :: React s (Singleton MouseMove) Point
mouseMove  = await MouseMoveReq \(OccMouseMove p) -> p

mousePos :: Sig s (Singleton MouseMove) Point ()
mousePos = repeat mouseMove

---------------------------------------------------------------------------
-- MOUSE SCROLL
---------------------------------------------------------------------------

data MouseScroll = MouseScrollReq deriving (Show, Eq, Ord)
numbered [t| MouseScroll |]
instance Request MouseScroll where
	data Occurred MouseScroll = OccMouseScroll Double Double deriving Show

mouseScroll :: React s (Singleton MouseScroll) (Double, Double)
mouseScroll = await MouseScrollReq \(OccMouseScroll dx dy) -> (dx, dy)

---------------------------------------------------------------------------
-- MOUSE EV
---------------------------------------------------------------------------

type MouseEv = MouseDown :- MouseUp :- MouseMove :- MouseScroll :- 'Nil
