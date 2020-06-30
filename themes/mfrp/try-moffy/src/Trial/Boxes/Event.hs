{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.Boxes.Event (
	-- * GENERAL
	SigG, ISigG,
	ReactG, GuiEv, Occurred(..),
	-- * TIME
	TimeEv, TryWait(..), sleep, DeltaTime(..), deltaTime,
	-- * MOUSE
	MouseEv, MouseDown, MouseUp, MouseBtn(..),
	leftClick, middleClick, rightClick, leftUp ) where
--	MouseMove, Point, mouseMove, DeleteEvent, deleteEvent ) where

import Data.Type.Set (Set(Nil), Singleton, (:-), (:+:), numbered)
import Data.Bool (bool)
import Data.Time (DiffTime)

import Moffy.React.Common (React, Request(..), await)
import Moffy.Sig
import Moffy.Event.Mouse

---------------------------------------------------------------------------

newtype TryWait = TryWaitReq { getTryWaitReq :: DiffTime } deriving (Show, Eq, Ord)
numbered 9 [t| TryWait |]
instance Request TryWait where
	data Occurred TryWait = OccTryWait DiffTime deriving (Show, Eq, Ord)

tryWait :: DiffTime -> React s (Singleton TryWait) DiffTime
tryWait t = await (TryWaitReq t) \(OccTryWait t') -> t'

sleep :: DiffTime -> React s (Singleton TryWait) ()
sleep t = tryWait t >>= \t' -> bool (sleep (t - t')) (pure ()) (t' == t)

data DeltaTime = DeltaTimeReq deriving (Show, Eq, Ord)
numbered 9 [t| DeltaTime |]
instance Request DeltaTime where
	data Occurred DeltaTime = OccDeltaTime DiffTime deriving (Show, Eq, Ord)

deltaTime :: React s (Singleton DeltaTime) DiffTime
deltaTime = await DeltaTimeReq \(OccDeltaTime t) -> t

type SigG s = Sig s GuiEv
type ISigG s = ISig s GuiEv
type ReactG s a = React s GuiEv a
type GuiEv = MouseEv :+: TimeEv
type TimeEv = TryWait :- DeltaTime :- 'Nil
