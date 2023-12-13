{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Boxes where

import Control.Moffy
import Control.Moffy.Samples.Event.Mouse qualified as Mouse
import Data.Type.Set
import Data.Bool

clickOn :: Mouse.Button -> React s (Singleton Mouse.Down) ()
clickOn b = bool (clickOn b) (pure ()) . (== b) =<< Mouse.down

leftClick, middleClick, rightClick :: React s (Singleton Mouse.Down) ()
leftClick = clickOn Mouse.ButtonPrimary
middleClick = clickOn Mouse.ButtonMiddle
rightClick = clickOn Mouse.ButtonSecondary
