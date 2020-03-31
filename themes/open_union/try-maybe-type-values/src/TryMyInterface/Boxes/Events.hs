{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryMyInterface.Boxes.Events where

import Data.Time

import MonadicFrp.MyInterface

data MouseDown = MouseDownReq deriving (Show, Eq, Ord)

numbered [t| MouseDown |]
instance Request MouseDown where
	data Occurred MouseDown = OccurredMouseDown [MouseBtn] deriving Show

data MouseBtn = MLeft | MMiddle | MRight | MUp | MDown deriving (Show, Eq)

mouseDown :: React (Singleton MouseDown) [MouseBtn]
mouseDown = await (MouseDownReq >+ UnionListNil) \ev ->
	let OccurredMouseDown mbs = extract ev in pure mbs

data TryWait = TryWaitReq DiffTime deriving (Show, Eq, Ord)

numbered [t| TryWait |]
instance Request TryWait where
	data Occurred TryWait = OccurredTryWait DiffTime deriving (Show, Eq, Ord)

tryWait :: DiffTime -> React (Singleton TryWait) DiffTime
tryWait t = await (TryWaitReq t >+ UnionListNil) \ev ->
	let OccurredTryWait t' = extract ev in pure t'

sleep :: DiffTime -> React (Singleton TryWait) ()
sleep t = do
	t' <- tryWait t
	if t' == t then pure () else sleep (t - t')

type ReactG = React GuiEv

type GuiEv = MouseDown :- TryWait :- 'Nil
