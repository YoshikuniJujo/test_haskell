{-# LANGUAGE DataKinds, TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.TryWindowKey where

import Control.Moffy
import Control.Moffy.Event.Window
import Control.Moffy.Event.Key
import Data.Type.Set
import Data.KeySym

tryWindow :: React s (WindowNew :- KeyDown :- 'Nil) KeySym
tryWindow = do
	w <- adjust windowNew
	adjust $ keyDown w
