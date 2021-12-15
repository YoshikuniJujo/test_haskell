{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Human where

import Foreign.C.Types

#include <human.h>

foreign import ccall "hm_left" left :: CInt -> CInt -> CInt
foreign import ccall "hm_right" right :: CInt -> CInt -> CInt
foreign import ccall "hm_top" top :: CInt -> CInt -> CInt
foreign import ccall "hm_bottom" bottom :: CInt -> CInt -> CInt

foreign import ccall "hm_x_from_left" xFromLeft :: CInt -> CInt
foreign import ccall "hm_x_from_right" xFromRight :: CInt -> CInt
foreign import ccall "hm_y_from_top" yFromTop :: CInt -> CInt
foreign import ccall "hm_y_from_bottom" yFromBottom :: CInt -> CInt

fieldWidth, fieldHeight :: CInt
fieldWidth = #{const FIELD_WIDTH}
fieldHeight = #{const FIELD_HEIGHT}

foreign import ccall "hm_field0_init" field0Init :: IO ()
foreign import ccall "hm_field0_draw" field0Draw :: IO ()
