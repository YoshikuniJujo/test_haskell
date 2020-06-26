{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Moffy.Event.Mouse where

import Data.Type.Set
import Data.Bool

import Moffy.React

data MouseDown = MouseDownReq deriving (Show, Eq, Ord)
data MouseBtn = MLeft | MMiddle | MRight | MUp | MDown deriving (Show, Eq, Ord)
numbered 9 [t| MouseDown |]
instance Request MouseDown where
	data Occurred MouseDown = OccMouseDown [MouseBtn]
		deriving (Show, Eq, Ord)

mouseDown :: React s (Singleton MouseDown) [MouseBtn]
mouseDown = await MouseDownReq \(OccMouseDown mbs) -> mbs

clickOn :: MouseBtn -> React s (Singleton MouseDown) ()
clickOn b = bool (clickOn b) (pure ()) . (b `elem`) =<< mouseDown

leftClick, middleClick, rightClick :: React s (Singleton MouseDown) ()
[leftClick, middleClick, rightClick] = clickOn <$> [MLeft, MMiddle, MRight]
