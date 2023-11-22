{-# LANGUAGE DataKinds, TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.TryWindowKey where

import Prelude hiding (repeat, until)

import Control.Monad
import Control.Moffy
import Control.Moffy.Event.Delete
import Control.Moffy.Event.Window
import Control.Moffy.Event.Mouse
import Control.Moffy.Event.Key
import Data.Type.Set
import Data.Type.Flip
import Data.Bool
import Data.KeySym

tryWindow :: React s (WindowNew :- KeyDown :- 'Nil) ()
tryWindow = do
	w <- adjust windowNew
	adjust $ clickKey w Xk_q

clickKey :: WindowId -> KeySym -> React s (Singleton KeyDown) ()
clickKey w k = keyDown w >>= bool (clickKey w k) (pure ()) . (== k)

tryWindowConfigure :: Sig s (DeleteEvent :- WindowEv :+: MouseMove :- MouseDown :- KeyEv :+: 'Nil) (Configure, Point, MouseBtn, KeySym) ()
tryWindowConfigure = do
	w <- waitFor $ adjust windowNew
	void . adjustSig
--		$ ((,) <$%> repeat (windowConfigure w) <*%> repeat (keyUp w))
		$ configureKeyUp w `until` (clickKey w Xk_q `first` deleteEvent w)

configureKeyUp :: WindowId -> Sig s (WindowConfigure :- MouseMove :- MouseDown :- KeyUp :- 'Nil) (Configure, Point, MouseBtn, KeySym) ()
configureKeyUp w = (,,,)
	<$%> repeat (adjust $ windowConfigure w)
	<*%> repeat (adjust $ mouseMove w)
	<*%> repeat (adjust $ mouseDown w)
	<*%> repeat (adjust $ keyUp w)
