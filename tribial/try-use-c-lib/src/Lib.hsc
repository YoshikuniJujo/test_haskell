{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Foreign.C.Types
import Foreign.C.Enum
import Data.Word

#include <human.h>

foreign import ccall "hm_left" c_hm_left :: CInt -> CInt -> CInt
foreign import ccall "hm_right" c_hm_right :: CInt -> CInt -> CInt
foreign import ccall "hm_top" c_hm_top :: CInt -> CInt -> CInt
foreign import ccall "hm_bottom" c_hm_bottom :: CInt -> CInt -> CInt

foreign import ccall "hm_init_field" c_hm_init_field :: IO ()
foreign import ccall "hm_draw_field" c_hm_draw_field :: IO ()

enum "DrawHumanResult" ''#{type HmDrawHumanResult} [''Show, ''Read] [
	("DrawHumanResultSuccess", #{const HM_DRAW_HUMAN_SUCCESS})
	]

foreign import ccall "hm_draw_human"
	c_hm_draw_human :: CInt -> CInt -> IO DrawHumanResult
