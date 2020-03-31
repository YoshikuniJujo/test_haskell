{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryMyInterface.Boxes.Events where

import MonadicFrp.MyInterface

data MouseDown = MouseDownReq deriving (Show, Eq, Ord)

numbered [t| MouseDown |]
instance Request MouseDown where
	data Occurred MouseDown = OccurredMouseDown [MouseBtn] deriving Show

data MouseBtn = MLeft | MMiddle | MRight | MUp | MDown deriving (Show, Eq)

mouseDown :: React (Singleton MouseDown) [MouseBtn]
mouseDown = await (MouseDownReq >+ UnionListNil) \ev ->
	let OccurredMouseDown mbs = extract ev in pure mbs

type ReactG = React GuiEv

type GuiEv = MouseDown :- 'Nil
