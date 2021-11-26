{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
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

newtype Field s = Field (ForeignPtr (Field s)) deriving Show

fieldNew :: IO (Field s)
fieldNew = Field <$> do
	p <- c_hm_field_new
	newForeignPtr p $ c_hm_field_destroy p

fieldDraw :: Field s -> IO ()
fieldDraw (Field ff) = withForeignPtr ff c_hm_field_draw

foreign import ccall "hm_field_new" c_hm_field_new :: IO (Ptr (Field s))
foreign import ccall "hm_field_draw" c_hm_field_draw :: Ptr (Field s) -> IO ()
foreign import ccall "hm_field_destroy" c_hm_field_destroy :: Ptr (Field s) -> IO ()

fieldPutHuman :: Field s -> CInt -> CInt -> IO ()
fieldPutHuman (Field ff) x y = withForeignPtr ff \pf -> do
	r <- c_hm_field_put_human pf x y
	case r of
		PutHumanResultSuccess -> pure ()
		PutHumanResultPartial -> throw PutHumanPartialError
		PutHumanResultOffscreen -> throw PutHumanOffscreenError
		PutHumanResult n -> throw $ PutHumanUnknownError n

foreign import ccall "hm_field_put_human"
	c_hm_field_put_human :: Ptr (Field s) -> CInt -> CInt -> IO PutHumanResult

newtype Image = Image (ForeignPtr Image) deriving Show

fieldGetImage :: Field s -> IO Image
fieldGetImage (Field ff) = Image <$> withForeignPtr ff \pf -> do
	pimg <- c_hm_field_get_image pf
	newForeignPtr pimg $ c_hm_image_destroy pimg

foreign import ccall "hm_field_get_image"
	c_hm_field_get_image :: Ptr (Field s) -> IO (Ptr Image)

foreign import ccall "hm_image_destroy" c_hm_image_destroy :: Ptr Image -> IO ()

imageDraw :: Image -> IO ()
imageDraw (Image fimg) = withForeignPtr fimg c_hm_image_draw

foreign import ccall "hm_image_draw" c_hm_image_draw :: Ptr Image -> IO ()
