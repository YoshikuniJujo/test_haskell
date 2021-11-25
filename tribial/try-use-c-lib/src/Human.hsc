{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Human where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
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

enum "PutHumanResult" ''#{type HmPutHumanResult} [''Show, ''Read] [
	("PutHumanResultSuccess", #{const HM_PUT_HUMAN_SUCCESS}),
	("PutHumanResultPartial", #{const HM_PUT_HUMAN_PARTIAL}),
	("PutHumanResultOffscreen", #{const HM_PUT_HUMAN_OFFSCREEN}) ]

field0DrawHuman :: CInt -> CInt -> IO ()
field0DrawHuman x y = do
	r <- c_hm_field0_draw_human x y
	case r of
		PutHumanResultSuccess -> pure ()
		PutHumanResultPartial -> throw PutHumanPartialError
		PutHumanResultOffscreen -> throw PutHumanOffscreenError
		PutHumanResult n -> throw $ PutHumanUnknownError n

foreign import ccall "hm_field0_draw_human"
	c_hm_field0_draw_human :: CInt -> CInt -> IO PutHumanResult

newtype Field = Field (ForeignPtr Field) deriving Show

fieldNew :: IO Field
fieldNew = Field <$> do
	p <- c_hm_field_new
	newForeignPtr p $ c_hm_field_destroy p

fieldDraw :: Field -> IO ()
fieldDraw (Field ff) = withForeignPtr ff c_hm_field_draw

foreign import ccall "hm_field_new" c_hm_field_new :: IO (Ptr Field)
foreign import ccall "hm_field_draw" c_hm_field_draw :: Ptr Field -> IO ()
foreign import ccall "hm_field_destroy" c_hm_field_destroy :: Ptr Field -> IO ()
