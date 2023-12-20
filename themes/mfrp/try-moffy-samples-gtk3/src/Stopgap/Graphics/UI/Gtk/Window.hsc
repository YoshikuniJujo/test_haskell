{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Stopgap.Graphics.UI.Gtk.Window where

import Foreign.Ptr
import Foreign.C.Enum
import Data.Word
import Stopgap.Data.Ptr
import Stopgap.System.GLib.Object qualified as G.Object
import Stopgap.Graphics.UI.Gtk.Widget qualified as Widget

#include <gtk/gtk.h>

enum "Type" ''#{type GtkWindowType} [''Show, ''Read, ''Eq] [
	("Toplevel", #{const GTK_WINDOW_TOPLEVEL}),
	("Popup", #{const GTK_WINDOW_POPUP}) ]

data WTag

newtype W = W (Ptr WTag) deriving Show

instance IsPtr W where type Tag W = WTag; toPtr (W p) = p; fromPtr = W
instance G.Object.IsO W where toO (W p) = G.Object.O $ castPtr p
instance Widget.IsW W where toW (W p) = Widget.W $ castPtr p

new :: Type -> IO W
new = c_gtk_window_new

foreign import ccall "gtk_window_new" c_gtk_window_new :: Type -> IO W
