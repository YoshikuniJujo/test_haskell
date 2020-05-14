{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trials.Boxes.Mouse (
	-- * GENERAL
	Occurred(..),
	-- * MOUSE EVENT
	MouseDown, MouseUp, MouseBtn(..), mouseDown, mouseUp,
	MouseMove, Point, mouseMove,
	-- * MOUSE HANDLE
	handleMouse
	) where

import Foreign.C.Types
import Data.Type.Set (Set(Nil), Singleton, numbered, (:-))
import Data.UnionSet
import Data.Time
import System.Exit

import MonadicFrp (React, Request(..), await, EvOccs)
import MonadicFrp.Handle
import Field

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

handleMouse :: Maybe DiffTime -> Field -> Handle' IO (MouseDown :- MouseUp :- MouseMove :- 'Nil)
handleMouse Nothing f _reqs = withNextEvent f $ eventToEvent f
handleMouse (Just prd) f _reqs = withNextEventTimeout'' f prd $ maybe (pure Nothing) (eventToEvent f)

eventToEvent :: Field -> Event -> IO (Maybe (EvOccs (MouseDown :- MouseUp :- MouseMove :- 'Nil)))
eventToEvent f = \case
	ButtonEvent {
		ev_event_type = 4, ev_button = eb,
		ev_x = x, ev_y = y }
		| Just b <- button eb -> pure . Just
			. expand $ mouseDownOcc x y [b]
	ButtonEvent {
		ev_event_type = 5, ev_button = eb,
		ev_x = x, ev_y = y }
		| Just b <- button eb -> pure . Just
			. expand $ mouseUpOcc x y [b]
	MotionEvent { ev_x = x, ev_y = y } ->
		pure . Just . expand $ mouseMoveOcc x y
	ExposeEvent {} ->
		Nothing <$ flushField f
	DestroyWindowEvent {} ->
		closeField f >> exitSuccess
	ev	| isDeleteEvent f ev -> Nothing <$ destroyField f
		| otherwise -> pure Nothing

mouseDownOcc ::
	CInt -> CInt -> [MouseBtn] -> EvOccs (MouseMove :- MouseDown :- 'Nil)
mouseDownOcc x y bs =OccMouseDown bs >- mouseMoveOcc x y

mouseUpOcc ::
	CInt -> CInt -> [MouseBtn] -> EvOccs (MouseMove :- MouseUp :- 'Nil)
mouseUpOcc x y bs = OccMouseUp bs >- mouseMoveOcc x y

mouseMoveOcc :: CInt -> CInt -> EvOccs (Singleton MouseMove)
mouseMoveOcc x y = singleton $ OccMouseMove (fromIntegral x, fromIntegral y)

withNextEventTimeout'' :: Field -> DiffTime -> (Maybe Event -> IO a) ->  IO a
withNextEventTimeout'' f = withNextEventTimeout' f . round . (* 1000000)

button :: Button -> Maybe MouseBtn
button = \case
	1 -> Just MLeft; 2 -> Just MMiddle; 3 -> Just MRight
	4 -> Just MUp; 5 -> Just MDown; _ -> Nothing
