{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

#include <X11/Xlib.h>

module ButtonEvent (ButtonEvent(..), buttonEvent) where

import Foreign.C.Types
import Graphics.X11.Xlib.Extras

data ButtonEvent = BtnEvent {
	buttonNumber :: ButtonNumber,
	pressOrRelease :: PR,
	position :: (CInt, CInt)
	} deriving Show

data PR = Press | Release deriving Show
data ButtonNumber = Button1 | Button2 | Button3 | Button4 | Button5
	deriving Show

buttonEvent :: Event -> Maybe ButtonEvent
buttonEvent ButtonEvent {
		ev_event_type = evt, ev_x = x, ev_y = y, ev_button = bn } =
	Just $ BtnEvent {
		buttonNumber = case () of
			_	| bn == #{const Button1} -> Button1
			_	| bn == #{const Button2} -> Button2
			_	| bn == #{const Button3} -> Button3
			_	| bn == #{const Button4} -> Button4
			_	| bn == #{const Button5} -> Button5
				| otherwise -> error "unrecognized button",
		pressOrRelease = case () of
			_	| evt == #{const ButtonPress} -> Press
				| evt == #{const ButtonRelease} -> Release
				| otherwise -> error "never occur",
		position = (x, y) }
buttonEvent _ = Nothing
