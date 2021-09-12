{-# LANGUAGE DataKinds, TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.TryWindowKey where

import Prelude hiding (repeat, until)

import Control.Monad
import Control.Moffy
import Control.Moffy.Event.Window
import Control.Moffy.Event.Key
import Data.Type.Set
import Data.Bool
import Data.KeySym

tryWindow :: React s (WindowNew :- KeyDown :- 'Nil) ()
tryWindow = do
	w <- adjust windowNew
	adjust $ clickKey w Xk_q

clickKey :: WindowId -> KeySym -> React s (Singleton KeyDown) ()
clickKey w k = keyDown w >>= bool (clickKey w k) (pure ()) . (== k)

tryWindowConfigure :: Sig s (WindowEv :+: KeyDown :- 'Nil) Configure ()
tryWindowConfigure = do
	w <- waitFor $ adjust windowNew
	void . adjustSig $ repeat (windowConfigure w) `until` clickKey w Xk_q
