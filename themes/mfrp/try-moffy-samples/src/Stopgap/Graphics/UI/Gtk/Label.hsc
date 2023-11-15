{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Stopgap.Graphics.UI.Gtk.Label where

import Foreign.Ptr
import Foreign.C.String

data LTag

data L = L (Ptr LTag) deriving Show

new :: String -> IO L
new txt = L <$> withCString txt c_gtk_label_new

foreign import ccall "gtk_label_new" c_gtk_label_new ::
	CString -> IO (Ptr LTag)
