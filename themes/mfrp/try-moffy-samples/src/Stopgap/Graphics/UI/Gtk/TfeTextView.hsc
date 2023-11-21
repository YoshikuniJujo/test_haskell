{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Stopgap.Graphics.UI.Gtk.TfeTextView where

import Foreign.Ptr
import Stopgap.Data.Ptr

import Stopgap.Graphics.UI.Gtk.Widget qualified as Gtk.Widget
import Stopgap.Graphics.UI.Gtk.TextView qualified as Gtk.TextView
import Stopgap.System.GLib.Object qualified as G.Object
import Stopgap.System.GLib.File qualified as G.File

data TTag

newtype T = T (Ptr TTag) deriving Show

instance IsPtr T where type Tag T = TTag; fromPtr = T; toPtr (T p) = p
instance G.Object.IsO T where toO (T t) = G.Object.O $ castPtr t
instance Gtk.Widget.IsW T where toW (T t) = Gtk.Widget.W $ castPtr t
instance Gtk.TextView.IsT T where toT (T t) = Gtk.TextView.T $ castPtr t

new :: IO T
new = T <$> c_tfe_text_view_new

foreign import ccall "tfe_text_view_new" c_tfe_text_view_new :: IO (Ptr TTag)

setFile :: T -> G.File.F -> IO ()
setFile (T t) (G.File.F f) = c_tfe_text_view_set_file t f

foreign import ccall "tfe_text_view_set_file" c_tfe_text_view_set_file ::
	Ptr TTag -> Ptr G.File.FTag -> IO ()

getFile :: T -> IO G.File.F
getFile (T t) = G.File.F <$> c_tfe_text_view_get_file t

foreign import ccall "tfe_text_view_get_file" c_tfe_text_view_get_file ::
	Ptr TTag -> IO (Ptr G.File.FTag)
