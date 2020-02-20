{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GuiEv (SigG, ISigG, ReactG, GuiEv(..), Event(..), Point, MouseBtn(..), Time) where

import Foreign.C.Types
import Data.Time

import Signal
import React
import Event

type SigG s = Sig s GuiEv
type ISigG s = ISig s GuiEv
type ReactG s a = React s GuiEv a

data GuiEv
	= MouseDown (Event [MouseBtn])
	| MouseUp (Event [MouseBtn])
	| MouseMove (Event Point)
	| DeltaTime (Event Time)
	| TryWait Time (Event Time)
	deriving (Eq, Show, Ord)

type Point = (CInt, CInt)
data MouseBtn = MLeft | MMiddle | MRight deriving (Show, Eq, Ord)
type Time = NominalDiffTime
