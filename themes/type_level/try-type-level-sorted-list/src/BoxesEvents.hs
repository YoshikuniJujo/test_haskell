{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module BoxesEvents (
	ReactG, SigG, ISigG, GuiEv,
	MouseDown, mouseDown, MouseUp, mouseUp, MouseMove, mouseMove,
	MouseBtn(..), Point,
	TryWait(..), sleep, DeltaTime(..), deltaTime,
	Occurred(..)
	) where

import Prelude hiding (head, tail, map, repeat, scanl, until)

import Data.List.NonEmpty (head)
import Data.Time (DiffTime)

import Sig (Sig, ISig)
import React (React(..), Request(..))
import OpenUnionValue (inj, extract)
import Sorted (Sorted(Nil), Singleton, (:-), numbered)

---------------------------------------------------------------------------

data MouseDown = MouseDownReq deriving Show

numbered [t| MouseDown |]
instance Request MouseDown where
	data Occurred MouseDown = OccurredMouseDown [MouseBtn] deriving Show

data MouseBtn = MLeft | MMiddle | MRight | MUp | MDown deriving (Show, Eq)

mouseDown :: React (Singleton MouseDown) [MouseBtn]
mouseDown = Await [inj MouseDownReq] \ev ->
	let OccurredMouseDown mbs = extract $ head ev in pure mbs

data MouseUp = MouseUpReq deriving Show

numbered [t| MouseUp |]
instance Request MouseUp where
	data Occurred MouseUp = OccurredMouseUp [MouseBtn] deriving Show

mouseUp :: React (Singleton MouseUp) [MouseBtn]
mouseUp = Await [inj MouseUpReq] \ev ->
	let OccurredMouseUp mbs = extract $ head ev in pure mbs

data MouseMove = MouseMoveReq deriving Show
type Point = (Integer, Integer)

numbered [t| MouseMove |]
instance Request MouseMove where
	data Occurred MouseMove = OccurredMouseMove Point deriving Show

mouseMove :: React (Singleton MouseMove) Point
mouseMove = Await [inj MouseMoveReq] \ev ->
	let OccurredMouseMove p = extract $ head ev in pure p

data TryWait = TryWaitReq DiffTime deriving Show

numbered [t| TryWait |]
instance Request TryWait where
	data Occurred TryWait = OccurredTryWait DiffTime deriving Show

tryWait :: DiffTime -> React (Singleton TryWait) DiffTime
tryWait t = Await [inj $ TryWaitReq t] \ev ->
	let OccurredTryWait t' = extract $ head ev in pure t'

sleep :: DiffTime -> React (Singleton TryWait) ()
sleep t = do
	t' <- tryWait t
	if t' == t then pure () else sleep (t - t')

data DeltaTime = DeltaTimeReq deriving Show

numbered [t| DeltaTime |]
instance Request DeltaTime where
	data Occurred DeltaTime = OccurredDeltaTime  DiffTime deriving Show

deltaTime :: React (Singleton DeltaTime) DiffTime
deltaTime = Await [inj DeltaTimeReq] \ev ->
	let OccurredDeltaTime dt = extract $ head ev in pure dt

type SigG = Sig GuiEv
type ISigG = ISig GuiEv
type ReactG = React GuiEv

type GuiEv = MouseDown :- MouseUp :- MouseMove :- TryWait :- DeltaTime :- 'Nil
