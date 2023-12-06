{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Stopgap.Graphics.UI.Gtk.TextBuffer where

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String

data TTag

newtype T = T (Ptr TTag) deriving Show

setText :: T -> String -> IO ()
setText (T t) txt = withCStringLen txt \(ctxt, len) ->
	c_gtk_text_buffer_set_text t ctxt $ fromIntegral len

foreign import ccall "gtk_text_buffer_set_text" c_gtk_text_buffer_set_text ::
	Ptr TTag -> CString -> CInt -> IO ()
