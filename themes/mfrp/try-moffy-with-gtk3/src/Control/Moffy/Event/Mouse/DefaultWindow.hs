{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Event.Mouse.DefaultWindow (
	M.MouseEv, M.MouseBtn, M.Point,
	M.MouseDown, mouseDown, clickOn, leftClick, middleClick, rightClick,
	M.MouseUp, mouseUp, releaseOn, leftUp, middleUp, rightUp,
	M.MouseMove, mouseMove, mousePos,

	M.MouseScroll, mouseScroll
	) where

import Prelude hiding (repeat)

import Control.Moffy
import Control.Moffy.Event.DefaultWindow
import Data.Type.Set
import Data.Bool

import qualified Control.Moffy.Event.Mouse as M

mouseDown :: React s (LoadDefaultWindow :- M.MouseDown :- 'Nil) M.MouseBtn
mouseDown = adjust . M.mouseDown =<< adjust loadDefaultWindow

clickOn :: M.MouseBtn -> React s (LoadDefaultWindow :- M.MouseDown :- 'Nil) ()
clickOn b = bool (clickOn b) (pure ()) . (== b) =<< mouseDown

leftClick, middleClick, rightClick :: React s (LoadDefaultWindow :- M.MouseDown :- 'Nil) ()
[leftClick, middleClick, rightClick] =
	clickOn <$> [M.ButtonLeft, M.ButtonMiddle, M.ButtonRight]

mouseUp :: React s (LoadDefaultWindow :- M.MouseUp :- 'Nil) M.MouseBtn
mouseUp = adjust . M.mouseUp =<< adjust loadDefaultWindow

releaseOn :: M.MouseBtn -> React s (LoadDefaultWindow :- M.MouseUp :- 'Nil) ()
releaseOn b = bool (releaseOn b) (pure ()) . (== b) =<< mouseUp

leftUp, middleUp, rightUp :: React s (LoadDefaultWindow :- M.MouseUp :- 'Nil) ()
[leftUp, middleUp, rightUp] =
	releaseOn <$> [M.ButtonLeft, M.ButtonMiddle, M.ButtonRight]

mouseMove :: React s (LoadDefaultWindow :- M.MouseMove :- 'Nil) M.Point
mouseMove = adjust . M.mouseMove =<< adjust loadDefaultWindow

mousePos :: Sig s (LoadDefaultWindow :- M.MouseMove :- 'Nil) M.Point r
mousePos = repeat mouseMove

mouseScroll :: React s (LoadDefaultWindow :- M.MouseScroll :- 'Nil) (Double, Double)
mouseScroll = adjust . M.mouseScroll =<< adjust loadDefaultWindow
