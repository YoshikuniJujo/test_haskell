{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Stopgap.Graphics.UI.Gtk.Box where

import Foreign.Ptr
import Foreign.C.Types
import Data.Word
import Data.Int
import Stopgap.Data.Ptr

import Stopgap.Graphics.UI.Gtk qualified as Gtk
import Stopgap.Graphics.UI.Gtk.Widget qualified as Gtk.Widget
import Stopgap.System.GLib.Object qualified as G.Object

#include <gtk/gtk.h>

data BTag

newtype B = B (Ptr BTag) deriving Show

instance IsPtr B where type Tag B = BTag; fromPtr = B; toPtr (B p) = p
instance G.Object.IsO B where toO (B p) = G.Object.O $ castPtr p
instance Gtk.Widget.IsW B where toW (B p) = Gtk.Widget.W $ castPtr p

new :: Gtk.Orientation -> Gtk.Pixel -> IO B
new (Gtk.Orientation o) (Gtk.Pixel p) = B <$> c_gtk_box_new o p

foreign import ccall "gtk_box_new" c_gtk_box_new ::
	#{type GtkOrientation} -> CInt -> IO (Ptr BTag)

append :: Gtk.Widget.IsW w => B -> w -> IO ()
append (B b) (Gtk.Widget.toW -> Gtk.Widget.W w) = c_gtk_box_append b w

foreign import ccall "gtk_box_append" c_gtk_box_append ::
	Ptr BTag -> Ptr Gtk.Widget.WTag -> IO ()

setHomogeneous :: B -> Bool -> IO ()
setHomogeneous (B bx) (boolToGBoolean -> bl) = c_gtk_box_set_homogeneous bx bl

boolToGBoolean :: Bool -> #{type gboolean}
boolToGBoolean = \case False -> #{const FALSE}; True -> #{const TRUE}

foreign import ccall "gtk_box_set_homogeneous" c_gtk_box_set_homogeneous ::
	Ptr BTag -> #{type gboolean} -> IO ()
