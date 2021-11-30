{-# LANGUAGE CApiFFI #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Foreign.Ptr
import Foreign.C.Types

foreign import ccall "foo" c_foo :: IO ()

foreign import capi "use-bin-tree-array.h value tree" c_tree :: Ptr ()
foreign import ccall "depth" c_depth :: Ptr () -> CInt -> CInt -> IO ()
