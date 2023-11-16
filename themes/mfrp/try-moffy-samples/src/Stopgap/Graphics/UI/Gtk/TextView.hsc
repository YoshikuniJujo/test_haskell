{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Stopgap.Graphics.UI.Gtk.TextView where

import Foreign.Ptr
import Stopgap.Data.Ptr
import Data.Word

import Stopgap.Graphics.UI.Gtk qualified as Gtk
import Stopgap.Graphics.UI.Gtk.Widget qualified as Gtk.Widget
import Stopgap.Graphics.UI.Gtk.TextBuffer qualified as Gtk.TextBuffer

#include <gtk/gtk.h>

data TTag

newtype T = T (Ptr TTag) deriving Show

instance IsPtr T where type Tag T = TTag; fromPtr = T; toPtr (T p) = p
instance Gtk.Widget.IsW T where toW (T t) = Gtk.Widget.W $ castPtr t

new :: IO T
new = T <$> c_gtk_text_view_new

foreign import ccall "gtk_text_view_new" c_gtk_text_view_new :: IO (Ptr TTag)

getBuffer :: T -> IO Gtk.TextBuffer.T
getBuffer (T t) = Gtk.TextBuffer.T <$> c_gtk_text_view_get_buffer t

foreign import ccall "gtk_text_view_get_buffer" c_gtk_text_view_get_buffer ::
	Ptr TTag -> IO (Ptr Gtk.TextBuffer.TTag)

setWrapMode :: T -> Gtk.WrapMode -> IO ()
setWrapMode (T t) (Gtk.WrapMode wm) = c_gtk_text_view_set_wrap_mode t wm

foreign import ccall "gtk_text_view_set_wrap_mode"
	c_gtk_text_view_set_wrap_mode ::
	Ptr TTag -> #{type GtkWrapMode} -> IO ()
