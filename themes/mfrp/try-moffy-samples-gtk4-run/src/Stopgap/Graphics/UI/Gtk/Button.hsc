{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Stopgap.Graphics.UI.Gtk.Button where

import Foreign.Ptr
import Foreign.C.String
import Stopgap.Data.Ptr

import Stopgap.Graphics.UI.Gtk.Widget qualified as Gtk.Widget
import Stopgap.System.GLib.Object qualified as G.Object

data BTag

newtype B = B (Ptr BTag) deriving Show

instance IsPtr B where type Tag B = BTag; fromPtr = B; toPtr (B p) = p
instance G.Object.IsO B where toO (B p) = G.Object.O $ castPtr p
instance Gtk.Widget.IsW B where toW (B p) = Gtk.Widget.W $ castPtr p

newWithLabel :: String -> IO B
newWithLabel lbl = B <$> withCString lbl c_gtk_button_new_with_label

foreign import ccall "gtk_button_new_with_label" c_gtk_button_new_with_label ::
	CString -> IO (Ptr BTag)

getLabel :: B -> IO String
getLabel (B b) = peekCString =<< c_gtk_button_get_label b

foreign import ccall "gtk_button_get_label" c_gtk_button_get_label ::
	Ptr BTag -> IO CString

setLabel :: B -> String -> IO ()
setLabel (B b) txt = withCString txt $ c_gtk_button_set_label b

foreign import ccall "gtk_button_set_label" c_gtk_button_set_label ::
	Ptr BTag -> CString -> IO ()
