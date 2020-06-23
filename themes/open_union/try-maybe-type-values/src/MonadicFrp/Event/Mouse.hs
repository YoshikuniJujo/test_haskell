{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MonadicFrp.Event.Mouse (
	-- * Occurred
	Occurred(..),
	-- * Mouse Event
	MouseEv, MouseBtn(..), Point,
	-- ** Mouse Down
	MouseDown, mouseDown, clickOn, leftClick, middleClick, rightClick,
	-- ** Mouse Up
	MouseUp, mouseUp, releaseOn, leftUp, middleUp, rightUp,
	-- ** Mouse Move
	MouseMove, mouseMove,
	-- ** Delete Event
	DeleteEvent, deleteEvent
	) where

import Data.Type.Set (Set(Nil), Singleton, (:-), numbered)
import Data.Bool (bool)

import MonadicFrp (Request(..), React, await)

---------------------------------------------------------------------------

data MouseDown = MouseDownReq deriving (Show, Eq, Ord)
data MouseBtn = MLeft | MMiddle | MRight | MUp | MDown deriving (Show, Eq, Ord)
numbered 9 [t| MouseDown |]
instance Request MouseDown where
	data Occurred MouseDown = OccMouseDown [MouseBtn] deriving (Show, Eq, Ord)

mouseDown :: React (Singleton MouseDown) [MouseBtn]
mouseDown = await MouseDownReq \(OccMouseDown mbs) -> mbs

clickOn :: MouseBtn -> React (Singleton MouseDown) ()
clickOn b = bool (clickOn b) (pure ()) . (b `elem`) =<< mouseDown

leftClick, middleClick, rightClick :: React (Singleton MouseDown) ()
[leftClick, middleClick, rightClick] = clickOn <$> [MLeft, MMiddle, MRight]

data MouseUp = MouseUpReq deriving (Show, Eq, Ord)
numbered 9 [t| MouseUp |]
instance Request MouseUp where
	data Occurred MouseUp = OccMouseUp [MouseBtn] deriving (Show, Eq, Ord)

mouseUp :: React (Singleton MouseUp) [MouseBtn]
mouseUp = await MouseUpReq \(OccMouseUp mbs) -> mbs

releaseOn :: MouseBtn -> React (Singleton MouseUp) ()
releaseOn b = bool (releaseOn b) (pure ()) . (b `elem`) =<< mouseUp

leftUp, middleUp, rightUp :: React (Singleton MouseUp) ()
[leftUp, middleUp, rightUp] = releaseOn <$> [MLeft, MMiddle, MRight]

data MouseMove = MouseMoveReq deriving (Show, Eq, Ord)
type Point = (Integer, Integer)
numbered 9 [t| MouseMove |]
instance Request MouseMove where
	data Occurred MouseMove = OccMouseMove Point deriving (Show, Eq, Ord)

mouseMove :: React (Singleton MouseMove) Point
mouseMove = await MouseMoveReq \(OccMouseMove p) -> p

data DeleteEvent = DeleteEventReq deriving (Show, Eq, Ord)
numbered 9 [t| DeleteEvent |]
instance Request DeleteEvent where
	data Occurred DeleteEvent = OccDeleteEvent deriving (Show, Eq, Ord)

deleteEvent :: React (Singleton DeleteEvent) ()
deleteEvent = await DeleteEventReq \OccDeleteEvent -> ()

type MouseEv = MouseDown :- MouseUp :- MouseMove :- DeleteEvent :- 'Nil
