{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.Boxes.BoxEv (
	-- * BoxEv
	SigB, ISigB, ReactB, BoxEv, BoxEvGen ) where

import Control.Moffy (Sig, ISig, React)
import Control.Moffy.Event.Time (TimeEv)
import Control.Moffy.Event.Delete (DeleteEvent)
import Control.Moffy.Event.Window
import Control.Moffy.Event.DefaultWindow
import Control.Moffy.Event.Key (KeyEv)
import Control.Moffy.Event.Mouse (MouseEv)
import Data.Type.Set ((:-), (:+:))

type SigB s = Sig s BoxEv
type ISigB s = ISig s BoxEv
type ReactB s r = React s BoxEv r
type BoxEv = DefaultWindowEv :+: BoxEvGen
type BoxEvGen = WindowEv :+: DeleteEvent :- KeyEv :+: MouseEv :+: TimeEv
