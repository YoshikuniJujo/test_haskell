{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Stopgap.Graphics.UI.Gtk.TextView where

import Foreign.Ptr
import Stopgap.Data.Ptr
import Data.Word
import Data.Int

import Stopgap.Graphics.UI.Gtk qualified as Gtk
import Stopgap.Graphics.UI.Gtk.Widget qualified as Gtk.Widget
import Stopgap.Graphics.UI.Gtk.TextBuffer qualified as Gtk.TextBuffer
import Stopgap.System.GLib.Object qualified as G.Object

#include <gtk/gtk.h>

class Gtk.Widget.IsW t => IsT t where toT :: t -> T

data TTag

newtype T = T (Ptr TTag) deriving Show

instance IsPtr T where type Tag T = TTag; fromPtr = T; toPtr (T p) = p
instance G.Object.IsO T where toO (T t) = G.Object.O $ castPtr t
instance Gtk.Widget.IsW T where toW (T t) = Gtk.Widget.W $ castPtr t
instance IsT T where toT = id

new :: IO T
new = T <$> c_gtk_text_view_new

foreign import ccall "gtk_text_view_new" c_gtk_text_view_new :: IO (Ptr TTag)

getBuffer :: IsT t => t -> IO Gtk.TextBuffer.T
getBuffer (toT -> T t) = Gtk.TextBuffer.T <$> c_gtk_text_view_get_buffer t

foreign import ccall "gtk_text_view_get_buffer" c_gtk_text_view_get_buffer ::
	Ptr TTag -> IO (Ptr Gtk.TextBuffer.TTag)

setWrapMode :: IsT t => t -> Gtk.WrapMode -> IO ()
setWrapMode (toT -> T t) (Gtk.WrapMode wm) = c_gtk_text_view_set_wrap_mode t wm

foreign import ccall "gtk_text_view_set_wrap_mode"
	c_gtk_text_view_set_wrap_mode ::
	Ptr TTag -> #{type GtkWrapMode} -> IO ()

setEditable :: IsT t => t -> Bool -> IO ()
setEditable (toT -> T t) b = c_gtk_text_view_set_editable t $ boolToGboolean b

boolToGboolean :: Bool -> #{type gboolean}
boolToGboolean False = #{const FALSE}
boolToGboolean True = #{const TRUE}

foreign import ccall "gtk_text_view_set_editable"
	c_gtk_text_view_set_editable ::
	Ptr TTag -> #{type gboolean} -> IO ()
