{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Stopgap.System.GLib.Object where

import Foreign.Ptr
import Stopgap.Data.Ptr

data OTag

newtype O = O (Ptr OTag) deriving Show

class IsPtr o => IsO o where toO :: o -> O

unref :: IsO o => o -> IO ()
unref (toO -> O o) = c_g_object_unref o

foreign import ccall "g_object_unref" c_g_object_unref :: Ptr OTag -> IO ()
