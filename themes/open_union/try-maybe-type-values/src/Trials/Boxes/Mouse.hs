{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trials.Boxes.Mouse (
	-- * GENERAL
	Occurred(..),
	-- * MOUSE
	MouseDown, MouseUp, MouseBtn(..), mouseDown, mouseUp,
	MouseMove, Point, mouseMove
	) where

import Data.Type.Set (Singleton, numbered)

import MonadicFrp (React, Request(..), await)

data MouseDown = MouseDownReq deriving (Show, Eq, Ord)
data MouseBtn = MLeft | MMiddle | MRight | MUp | MDown deriving (Show, Eq)
numbered 8 [t| MouseDown |]
instance Request MouseDown where
	data Occurred MouseDown = OccMouseDown [MouseBtn] deriving Show

mouseDown :: React (Singleton MouseDown) [MouseBtn]
mouseDown = await MouseDownReq \(OccMouseDown mbs) -> mbs

data MouseUp = MouseUpReq deriving (Show, Eq, Ord)
numbered 8 [t| MouseUp |]
instance Request MouseUp where
	data Occurred MouseUp = OccMouseUp [MouseBtn] deriving Show

mouseUp :: React (Singleton MouseUp) [MouseBtn]
mouseUp = await MouseUpReq \(OccMouseUp mbs) -> mbs

data MouseMove = MouseMoveReq deriving (Show, Eq, Ord)
type Point = (Integer, Integer)
numbered 8 [t| MouseMove |]
instance Request MouseMove where
	data Occurred MouseMove = OccMouseMove Point deriving Show

mouseMove :: React (Singleton MouseMove) Point
mouseMove = await MouseMoveReq \(OccMouseMove p) -> p
