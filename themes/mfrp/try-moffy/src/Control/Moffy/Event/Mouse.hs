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
	MouseDown(..), pattern OccMouseDown,
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
	data Occurred MouseUp = OccMouseUp WindowId MouseBtn deriving Show

mouseUp :: WindowId -> React s (Singleton MouseUp) MouseBtn
mouseUp wid0 = maybe (mouseUp wid0) pure =<<
	await MouseUpReq \(OccMouseUp wid b) -> bool Nothing (Just b) (wid == wid0)

releaseOn :: WindowId -> MouseBtn -> React s (Singleton MouseUp) ()
releaseOn wid b = bool (releaseOn wid b) (pure ()) . (== b) =<< mouseUp wid

leftUp, middleUp, rightUp :: WindowId -> React s (Singleton MouseUp) ()
[leftUp, middleUp, rightUp] =
	flip releaseOn <$> [ButtonLeft, ButtonMiddle, ButtonRight]

---------------------------------------------------------------------------
-- MOUSE MOVE
---------------------------------------------------------------------------

data MouseMove = MouseMoveReq deriving (Show, Eq, Ord)
numbered [t| MouseMove |]
instance Request MouseMove where
	data Occurred MouseMove = OccMouseMove WindowId Point deriving Show

type Point = (Double, Double)

mouseMove :: WindowId -> React s (Singleton MouseMove) Point
mouseMove wid0 = maybe (mouseMove wid0) pure =<<
	await MouseMoveReq \(OccMouseMove wid p) -> bool Nothing (Just p) (wid == wid0)

mousePos :: WindowId -> Sig s (Singleton MouseMove) Point r
mousePos = repeat . mouseMove

---------------------------------------------------------------------------
-- MOUSE SCROLL
---------------------------------------------------------------------------

data MouseScroll = MouseScrollReq deriving (Show, Eq, Ord)
numbered [t| MouseScroll |]
instance Request MouseScroll where
	data Occurred MouseScroll = OccMouseScroll WindowId Double Double deriving Show

mouseScroll :: WindowId -> React s (Singleton MouseScroll) (Double, Double)
mouseScroll wid0 = maybe (mouseScroll wid0) pure =<<
	await MouseScrollReq \(OccMouseScroll wid dx dy) -> bool Nothing (Just (dx, dy)) (wid == wid0)

---------------------------------------------------------------------------
-- MOUSE EV
---------------------------------------------------------------------------

type MouseEv = MouseDown :- MouseUp :- MouseMove :- MouseScroll :- 'Nil
