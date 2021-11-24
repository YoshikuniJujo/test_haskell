{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Foreign.C.Types

foreign import ccall "left" c_left :: CInt -> CInt -> CInt
foreign import ccall "right" c_right :: CInt -> CInt -> CInt
foreign import ccall "top" c_top :: CInt -> CInt -> CInt
foreign import ccall "bottom" c_bottom :: CInt -> CInt -> CInt

foreign import ccall "init_field" c_init_field :: IO ()
foreign import ccall "draw_field" c_draw_field :: IO ()

foreign import ccall "draw_human" c_draw_human :: CInt -> CInt -> IO ()
