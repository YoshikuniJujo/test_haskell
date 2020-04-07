{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Boxes.Events (
	ReactG, SigG, ISigG, GuiEv,
	MouseDown(..), MouseUp(..), MouseMove(..), TryWait(..), DeltaTime(..),
	Occurred(..), MouseBtn(..), Point,
	mouseDown, mouseUp, mouseMove, sleep, deltaTime ) where

import Data.Time

import MonadicFrp.Sig
import MonadicFrp.React
import Data.UnionSet
import Data.Type.Set

data MouseDown = MouseDownReq deriving (Show, Eq, Ord)

numbered [t| MouseDown |]
instance Request MouseDown where
	data Occurred MouseDown = OccurredMouseDown [MouseBtn] deriving Show

data MouseBtn = MLeft | MMiddle | MRight | MUp | MDown deriving (Show, Eq)

mouseDown :: React (Singleton MouseDown) [MouseBtn]
mouseDown = await_ (MouseDownReq >- Empty) \ev ->
	let OccurredMouseDown mbs = extract ev in pure mbs

data MouseUp = MouseUpReq deriving (Show, Eq, Ord)

numbered [t| MouseUp |]
instance Request MouseUp where
	data Occurred MouseUp = OccurredMouseUp [MouseBtn] deriving Show

mouseUp :: React (Singleton MouseUp) [MouseBtn]
mouseUp = await_ (MouseUpReq >- Empty) \ev ->
	let OccurredMouseUp mbs = extract ev in pure mbs

data MouseMove = MouseMoveReq deriving (Show, Eq, Ord)
type Point = (Integer, Integer)

numbered [t| MouseMove |]
instance Request MouseMove where
	data Occurred MouseMove = OccurredMouseMove Point deriving Show

mouseMove :: React (Singleton MouseMove) Point
mouseMove = await_ (MouseMoveReq >- Empty) \ev ->
	let OccurredMouseMove p = extract ev in pure p

data TryWait = TryWaitReq DiffTime deriving (Show, Eq, Ord)

numbered [t| TryWait |]
instance Request TryWait where
	data Occurred TryWait = OccurredTryWait DiffTime deriving (Show, Eq, Ord)

tryWait :: DiffTime -> React (Singleton TryWait) DiffTime
tryWait t = await_ (TryWaitReq t >- Empty) \ev ->
	let OccurredTryWait t' = extract ev in pure t'

sleep :: DiffTime -> React (Singleton TryWait) ()
sleep t = do
	t' <- tryWait t
	if t' == t then pure () else sleep (t - t')

data DeltaTime = DeltaTimeReq deriving (Show, Eq, Ord)

numbered [t| DeltaTime |]
instance Request DeltaTime where
	data Occurred DeltaTime = OccurredDeltaTime DiffTime deriving (Show, Eq, Ord)

deltaTime :: React (Singleton DeltaTime) DiffTime
deltaTime = await_ (DeltaTimeReq >- Empty) \ev ->
	let OccurredDeltaTime t = extract ev in pure t

type SigG = Sig GuiEv
type ISigG = ISig GuiEv
type ReactG = React GuiEv

type GuiEv = MouseDown :- MouseUp :- MouseMove :- TryWait :- DeltaTime :- 'Nil
