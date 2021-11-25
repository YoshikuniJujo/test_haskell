{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Human where

import Foreign.C.Types
import Foreign.C.Enum
import Control.Exception
import Data.Word

import Human.Exception

#include <human.h>

foreign import ccall "hm_left" c_hm_left :: CInt -> CInt -> CInt
foreign import ccall "hm_right" c_hm_right :: CInt -> CInt -> CInt
foreign import ccall "hm_top" c_hm_top :: CInt -> CInt -> CInt
foreign import ccall "hm_bottom" c_hm_bottom :: CInt -> CInt -> CInt

foreign import ccall "hm_field0_init" c_hm_field0_init :: IO ()
foreign import ccall "hm_field0_draw" c_hm_field0_draw :: IO ()

enum "DrawHumanResult" ''#{type HmDrawHumanResult} [''Show, ''Read] [
	("DrawHumanResultSuccess", #{const HM_DRAW_HUMAN_SUCCESS}),
	("DrawHumanResultPartial", #{const HM_DRAW_HUMAN_PARTIAL}),
	("DrawHumanResultOffscreen", #{const HM_DRAW_HUMAN_OFFSCREEN}) ]

field0DrawHuman :: CInt -> CInt -> IO ()
field0DrawHuman x y = do
	r <- c_hm_field0_draw_human x y
	case r of
		DrawHumanResultSuccess -> pure ()
		DrawHumanResultPartial -> throw DrawHumanPartialError
		DrawHumanResultOffscreen -> throw DrawHumanOffscreenError
		DrawHumanResult n -> throw $ DrawHumanUnknownError n

foreign import ccall "hm_field0_draw_human"
	c_hm_field0_draw_human :: CInt -> CInt -> IO DrawHumanResult
