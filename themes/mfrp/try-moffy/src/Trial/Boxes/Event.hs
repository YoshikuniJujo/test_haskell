{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.Boxes.Event (
	-- * GENERAL
	SigG, ISigG,
	ReactG, BoxEv, -- pattern OccDeltaTime, pattern OccTryWait,
	-- * TIME
	TimeEv, TryWait(..), sleep, DeltaTime(..), deltaTime,
	-- * MOUSE
	GuiEv, MouseDown, MouseUp, MouseBtn(..),
	leftClick, middleClick, rightClick, leftUp ) where

import Control.Moffy
import Data.Type.Set ((:+:))

import Control.Moffy.Event.Mouse
import Control.Moffy.Event.Time
import Control.Moffy.Handle.XField

---------------------------------------------------------------------------

type SigG s = Sig s BoxEv
type ISigG s = ISig s BoxEv
type ReactG s a = React s BoxEv a
type BoxEv = GuiEv :+: TimeEv
