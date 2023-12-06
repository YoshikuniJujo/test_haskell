{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Stopgap.Graphics.UI.Gtk.GestureClick where

import Foreign.Ptr
import Foreign.C.Enum
import Stopgap.Data.Ptr
import Data.Word

import Stopgap.Graphics.UI.Gtk.EventController qualified as Gtk.EventController
import Stopgap.Graphics.UI.Gtk.Gesture qualified as Gtk.Gesture
import Stopgap.System.GLib.Object qualified as G.Object

#include <gtk/gtk.h>

data GTag

newtype G = G (Ptr GTag) deriving Show

instance IsPtr G where type Tag G = GTag; fromPtr = G; toPtr (G p) = p
instance G.Object.IsO G where toO (G g) = G.Object.O $ castPtr g

instance Gtk.EventController.IsE G where
	toE (G g) = Gtk.EventController.E $ castPtr g

instance Gtk.Gesture.IsG G where toG (G g) = Gtk.Gesture.G $ castPtr g

new :: IO G
new = G <$> c_gtk_gesture_click_new

foreign import ccall "gtk_gesture_click_new" c_gtk_gesture_click_new ::
	IO (Ptr GTag)

enum "Button" ''#{type guint} [''Show, ''Read] [
	("ButtonPrimary", #{const GDK_BUTTON_PRIMARY}),
	("ButtonMiddle", #{const GDK_BUTTON_MIDDLE}),
	("ButtonSecondary", #{const GDK_BUTTON_SECONDARY})
	]

setButton :: G -> Button -> IO ()
setButton (G g) (Button b) = c_gtk_gesture_single_set_button g b

foreign import ccall "gtk_gesture_single_set_button"
	c_gtk_gesture_single_set_button ::
	Ptr GTag -> #{type guint} -> IO ()
