{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GuiEv (SigG, ISigG, ReactG, GuiEv(..), Event(..), Point, MouseBtn(..), Time) where

import Foreign.C.Types
import Data.Time

import Signal
import React

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

data Event a = Request | Occurred a deriving Show

instance Ord a => Eq (Event a) where a == b = a `compare` b == EQ

instance Ord a => Ord (Event a) where
	Occurred x `compare` Occurred y = x `compare` y
	Request `compare` _ = EQ
	_ `compare` Request = EQ

type Point = (CInt, CInt)
data MouseBtn = MLeft | MMiddle | MRight deriving (Show, Eq, Ord)
type Time = NominalDiffTime
