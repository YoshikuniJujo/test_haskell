{-# LANGUAGE DataKinds, TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Event.Gui where

import Control.Moffy.Event.Window
import Control.Moffy.Event.Delete
import Control.Moffy.Event.Mouse
import Control.Moffy.Event.Cursor
import Control.Moffy.Event.Key
import Control.Moffy.Event.CalcTextExtents
import Data.Type.Set

type GuiEv =
	WindowEv :+: DeleteEvent :- MouseEv :+: CursorEv :+: KeyEv :+:
	CalcTextExtents :- 'Nil

type NoTextExtents =
	WindowEv :+: DeleteEvent :- MouseEv :+: CursorEv :+: KeyEv
