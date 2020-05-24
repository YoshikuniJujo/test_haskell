{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trials.Boxes.Event (
	-- * GENERAL
	SigG, ISigG, ReactG, GuiEv, Occurred(..),
	-- * TIME
	TimeEv, TryWait(..), sleep, DeltaTime(..), deltaTime,
	-- * MOUSE
	MouseEv, MouseDown, MouseUp, MouseBtn(..),
	leftClick, middleClick, rightClick, leftUp,
	MouseMove, Point, mouseMove, DeleteEvent, deleteEvent ) where

import Data.Type.Set (Set(Nil), Singleton, (:-), (:+:), numbered)
import Data.Bool (bool)
import Data.Time (DiffTime)

import MonadicFrp (Sig, ISig, React, Request(..), await)
import MonadicFrp.Events.Mouse (
	MouseEv, MouseDown, MouseUp, MouseBtn(..), MouseMove,
	Point, DeleteEvent,
	leftClick, middleClick, rightClick, leftUp, mouseMove, deleteEvent )

---------------------------------------------------------------------------

data TryWait = TryWaitReq { getTryWaitReq :: DiffTime } deriving (Show, Eq, Ord)
numbered 9 [t| TryWait |]
instance Request TryWait where
	data Occurred TryWait = OccTryWait DiffTime deriving (Show, Eq, Ord)

tryWait :: DiffTime -> React (Singleton TryWait) DiffTime
tryWait t = await (TryWaitReq t) \(OccTryWait t') -> t'

sleep :: DiffTime -> React (Singleton TryWait) ()
sleep t = tryWait t >>= \t' -> bool (sleep (t - t')) (pure ()) (t' == t)

data DeltaTime = DeltaTimeReq deriving (Show, Eq, Ord)
numbered 9 [t| DeltaTime |]
instance Request DeltaTime where
	data Occurred DeltaTime = OccDeltaTime DiffTime deriving (Show, Eq, Ord)

deltaTime :: React (Singleton DeltaTime) DiffTime
deltaTime = await DeltaTimeReq \(OccDeltaTime t) -> t

type SigG = Sig GuiEv
type ISigG = ISig GuiEv
type ReactG = React GuiEv
type GuiEv = MouseEv :+: TimeEv
type TimeEv = TryWait :- DeltaTime :- 'Nil
