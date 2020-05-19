{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MonadicFrp.Events.Mouse (
	-- * GENERAL
	Occurred(..),
	-- * MOUSE EVENT
	MouseEv, MouseDown, MouseUp, MouseBtn(..), mouseDown, mouseUp,
	MouseMove, Point, mouseMove, DeleteEvent, deleteEvent
	) where

import Data.Type.Set (Set(Nil), Singleton, numbered, (:-))

import MonadicFrp

data MouseDown = MouseDownReq deriving (Show, Eq, Ord)
data MouseBtn = MLeft | MMiddle | MRight | MUp | MDown deriving (Show, Eq, Ord)
numbered 8 [t| MouseDown |]
instance Request MouseDown where
	data Occurred MouseDown = OccMouseDown [MouseBtn] deriving (Show, Eq, Ord)

mouseDown :: React (Singleton MouseDown) [MouseBtn]
mouseDown = await MouseDownReq \(OccMouseDown mbs) -> mbs

data MouseUp = MouseUpReq deriving (Show, Eq, Ord)
numbered 8 [t| MouseUp |]
instance Request MouseUp where
	data Occurred MouseUp = OccMouseUp [MouseBtn] deriving (Show, Eq, Ord)

mouseUp :: React (Singleton MouseUp) [MouseBtn]
mouseUp = await MouseUpReq \(OccMouseUp mbs) -> mbs

data MouseMove = MouseMoveReq deriving (Show, Eq, Ord)
type Point = (Integer, Integer)
numbered 8 [t| MouseMove |]
instance Request MouseMove where
	data Occurred MouseMove = OccMouseMove Point deriving (Show, Eq, Ord)

mouseMove :: React (Singleton MouseMove) Point
mouseMove = await MouseMoveReq \(OccMouseMove p) -> p

data DeleteEvent = DeleteEventReq deriving (Show, Eq, Ord)
numbered 8 [t| DeleteEvent |]
instance Request DeleteEvent where
	data Occurred DeleteEvent = OccDeleteEvent deriving (Show, Eq, Ord)

deleteEvent :: React (Singleton DeleteEvent) ()
deleteEvent = await DeleteEventReq \OccDeleteEvent -> ()

type MouseEv = MouseDown :- MouseUp :- MouseMove :- DeleteEvent :- 'Nil
