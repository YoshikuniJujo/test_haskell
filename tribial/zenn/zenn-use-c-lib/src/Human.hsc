{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Human where

import Foreign.C.Types
import Foreign.C.Enum
import Control.Exception
import Data.Word

import Human.Exception

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

enum "PutHumanResult" ''#{type HmPutHumanResult} [''Show, ''Read] [
	("PutHumanResultSuccess", #{const HM_PUT_HUMAN_SUCCESS}),
	("PutHumanResultPartial", #{const HM_PUT_HUMAN_PARTIAL}),
	("PutHumanResultOffscreen", #{const HM_PUT_HUMAN_OFFSCREEN}) ]

foreign import ccall "hm_field0_draw_human"
	c_hm_field0_draw_human :: CInt -> CInt -> IO PutHumanResult

field0DrawHuman :: CInt -> CInt -> IO ()
field0DrawHuman x y = c_hm_field0_draw_human x y >>= \case
	PutHumanResultSuccess -> pure ()
	PutHumanResultPartial -> throw PutHumanPartialError
	PutHumanResultOffscreen -> throw PutHumanOffscreenError
	PutHumanResult n -> throw $ PutHumanUnknownError n
