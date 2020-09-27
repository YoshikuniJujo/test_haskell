{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Event.Mouse.DefaultWindow (
	M.MouseEv, M.MouseDown, M.MouseBtn, mouseDown, leftClick, rightClick,

	M.mousePos, M.leftUp, middleClick, M.Point, M.MouseUp, M.MouseMove, M.mouseMove,
	M.MouseScroll, M.mouseScroll
	) where

import GHC.Stack
import Control.Moffy
import Control.Moffy.Event.Window
import Control.Moffy.Event.DefaultWindow
import Data.Type.Set
import Data.Bool

import qualified Control.Moffy.Event.Mouse as M

checkNoDefault :: HasCallStack => Maybe WindowId -> WindowId
checkNoDefault = \case Nothing -> error "No default window"; Just wid -> wid

mouseDown :: React s (LoadDefaultWindow :- M.MouseDown :- 'Nil) M.MouseBtn
mouseDown = adjust . M.mouseDown =<< adjust loadDefaultWindow

clickOn :: M.MouseBtn -> React s (LoadDefaultWindow :- M.MouseDown :- 'Nil) ()
clickOn b = bool (clickOn b) (pure ()) . (== b) =<< mouseDown

leftClick, middleClick, rightClick :: React s (LoadDefaultWindow :- M.MouseDown :- 'Nil) ()
[leftClick, middleClick, rightClick] =
	clickOn <$> [M.ButtonLeft, M.ButtonMiddle, M.ButtonRight]
