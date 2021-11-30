{-# LANGUAGE CApiFFI #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Foreign.Ptr
import Foreign.C.Types

foreign import ccall "foo" c_foo :: IO ()

foreign import capi "use-bin-tree-array.h value tree" c_tree :: Ptr ()
foreign import ccall "depth" c_depth :: Ptr () -> CInt -> CInt -> IO ()

foreign import ccall "normalize" c_normalize :: CInt -> CInt

foreign import ccall "test_pointer_calc" c_test_pointer_calc :: IO ()

foreign import ccall "allocate_memory" c_allocate_memory :: CInt -> IO (Ptr a)
foreign import ccall "free_memory" c_free_memory :: Ptr a -> IO ()
foreign import ccall "draw_memory" c_draw_memory :: IO ()
