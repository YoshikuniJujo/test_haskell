{-# LANGUAGE DataKinds, TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.TryWindowKey where

import Control.Moffy
import Control.Moffy.Event.Window
import Control.Moffy.Event.Key
import Data.Type.Set

tryWindow :: React s (WindowNew :- KeyDown :- 'Nil) Key
tryWindow = do
	w <- adjust windowNew
	adjust $ keyDown w
