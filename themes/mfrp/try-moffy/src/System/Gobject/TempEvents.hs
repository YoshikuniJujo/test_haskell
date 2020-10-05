{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module System.Gobject.TempEvents where

import System.Gobject.Hierarchy
import System.Gobject.SignalConnect
import System.Gobject.TryDeleteEvent

data KeyEvent = KeyPressEvent | KeyReleaseEvent deriving Show

instance Signal KeyEvent where
	type Reciever KeyEvent = GtkWidget
