module System.Glib.GObject where

import Foreign.Ptr

foreign import ccall "g_object_unref" c_g_object_unref :: Ptr a -> IO ()
