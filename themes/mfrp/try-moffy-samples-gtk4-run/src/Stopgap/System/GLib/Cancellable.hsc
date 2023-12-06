{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Stopgap.System.GLib.Cancellable where

import Foreign.Ptr

data CTag

newtype C = C (Ptr CTag) deriving Show

new :: IO C
new = C <$> c_g_cancellable_new

foreign import ccall "g_cancellable_new" c_g_cancellable_new :: IO (Ptr CTag)

cancel :: C -> IO ()
cancel (C c) = c_g_cancellable_cancel c

foreign import ccall "g_cancellable_cancel" c_g_cancellable_cancel ::
	Ptr CTag -> IO()
