{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Foreign.C.Types

foreign import ccall "left" c_left :: CInt -> CInt -> CInt
foreign import ccall "right" c_right :: CInt -> CInt -> CInt
foreign import ccall "top" c_top :: CInt -> CInt -> CInt
foreign import ccall "bottom" c_bottom :: CInt -> CInt -> CInt
