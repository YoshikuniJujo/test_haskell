{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryMemory where

import Foreign.Ptr
import Foreign.C.Types

foreign import ccall "allocate_memory" allocateMemory :: CInt -> IO (Ptr a)
foreign import ccall "free_memory" freeMemory :: Ptr a -> IO CInt

foreign import ccall "draw_memory" drawMemory :: IO ()
