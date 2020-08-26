{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.Boxes.BoxEv where

import Control.Moffy
import Control.Moffy.Event.Time
import Control.Moffy.Event.Delete
import Control.Moffy.Event.Key
import Control.Moffy.Event.Mouse
import Data.Type.Set

type SigG s = Sig s BoxEv
type ISigG s = ISig s BoxEv
type ReactG s a = React s BoxEv a
type BoxEv = DeleteEvent :- KeyEv :+: MouseEv :+: TimeEv
