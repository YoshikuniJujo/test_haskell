{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trials.Boxes.Events (
	-- * GENERAL
	SigG, ISigG, ReactG, GuiEv, Occurred(..),
	-- * TIME
	TryWait(..), sleep, DeltaTime(..), deltaTime,
	-- * MOUSE
	MouseDown, MouseUp, MouseBtn(..), mouseDown, mouseUp,
	MouseMove, Point, mouseMove
	) where

import Data.Type.Set (Set(Nil), Singleton, (:-), numbered)
import Data.Bool (bool)
import Data.Time (DiffTime)

import MonadicFrp (Sig, ISig, React, Request(..), await)
import Trials.Boxes.Mouse

data TryWait = TryWaitReq { getTryWaitReq :: DiffTime } deriving (Show, Eq, Ord)
numbered 8 [t| TryWait |]
instance Request TryWait where
	data Occurred TryWait = OccTryWait DiffTime deriving (Show, Eq, Ord)

tryWait :: DiffTime -> React (Singleton TryWait) DiffTime
tryWait t = await (TryWaitReq t) \(OccTryWait t') -> t'

sleep :: DiffTime -> React (Singleton TryWait) ()
sleep t = tryWait t >>= \t' -> bool (sleep (t - t')) (pure ()) (t' == t)

data DeltaTime = DeltaTimeReq deriving (Show, Eq, Ord)
numbered 8 [t| DeltaTime |]
instance Request DeltaTime where
	data Occurred DeltaTime = OccDeltaTime DiffTime deriving (Show, Eq, Ord)

deltaTime :: React (Singleton DeltaTime) DiffTime
deltaTime = await DeltaTimeReq \(OccDeltaTime t) -> t

type SigG = Sig GuiEv
type ISigG = ISig GuiEv
type ReactG = React GuiEv

type GuiEv = MouseDown :- MouseUp :- MouseMove :- TryWait :- DeltaTime :- 'Nil
