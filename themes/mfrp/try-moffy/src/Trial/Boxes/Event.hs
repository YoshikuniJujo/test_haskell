{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.Boxes.Event (
	-- * GENERAL
	SigG, ISigG,
	ReactG, BoxEv, Occurred(..),
	-- * TIME
	TimeEv, TryWait(..), sleep, DeltaTime(..), deltaTime,
	-- * MOUSE
	GuiEv, MouseDown, MouseUp, MouseBtn(..),
	leftClick, middleClick, rightClick, leftUp ) where
--	MouseMove, Point, mouseMove, DeleteEvent, deleteEvent ) where

import Control.Moffy
import Data.Type.Set (Set(Nil), Singleton, (:-), (:+:), numbered)
import Data.Bool (bool)
import Data.Time (DiffTime)

import Control.Moffy.Handle.XField
import Control.Moffy.Event.Mouse
import Control.Moffy.Event.Delete

---------------------------------------------------------------------------

newtype TryWait = TryWaitReq { getTryWaitReq :: DiffTime } deriving (Show, Eq, Ord)
numbered 64 [t| TryWait |]
instance Request TryWait where
	data Occurred TryWait = OccTryWait DiffTime deriving (Show, Eq, Ord)

tryWait :: DiffTime -> React s (Singleton TryWait) DiffTime
tryWait t = await (TryWaitReq t) \(OccTryWait t') -> t'

sleep :: DiffTime -> React s (Singleton TryWait) ()
sleep t = tryWait t >>= \t' -> bool (sleep (t - t')) (pure ()) (t' == t)

data DeltaTime = DeltaTimeReq deriving (Show, Eq, Ord)
numbered 64 [t| DeltaTime |]
instance Request DeltaTime where
	data Occurred DeltaTime = OccDeltaTime DiffTime deriving (Show, Eq, Ord)

deltaTime :: React s (Singleton DeltaTime) DiffTime
deltaTime = await DeltaTimeReq \(OccDeltaTime t) -> t

type SigG s = Sig s BoxEv
type ISigG s = ISig s BoxEv
type ReactG s a = React s BoxEv a
type BoxEv = GuiEv :+: TimeEv
type TimeEv = TryWait :- DeltaTime :- 'Nil
