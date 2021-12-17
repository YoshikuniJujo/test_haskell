{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Human where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.C.Types
import Foreign.C.Enum
import Control.Monad.ST
import Control.Monad.ST.Unsafe
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

newtype Field s = Field (ForeignPtr (Field s)) deriving Show

foreign import ccall "hm_field_new" c_hm_field_new :: IO (Ptr (Field s))

foreign import ccall "hm_field_destroy"
	c_hm_field_destroy :: Ptr (Field s) -> IO ()

fieldNewRaw :: IO (Field s)
fieldNewRaw = Field <$> do
	p <- c_hm_field_new
	newForeignPtr p $ c_hm_field_destroy p

foreign import ccall "hm_field_clear" c_hm_field_clear :: Ptr (Field s) -> IO ()
foreign import ccall "hm_field_draw" c_hm_field_draw :: Ptr (Field s) -> IO ()

foreign import ccall "hm_field_put_human" c_hm_field_put_human ::
	Ptr (Field s) -> CInt -> CInt -> IO PutHumanResult

fieldClearRaw :: Field s -> IO ()
fieldClearRaw (Field ff) = withForeignPtr ff c_hm_field_clear

fieldDrawRaw :: Field s -> IO ()
fieldDrawRaw (Field ff) = withForeignPtr ff c_hm_field_draw

fieldPutHumanRaw :: Field s -> CInt -> CInt -> IO ()
fieldPutHumanRaw (Field ff) x y = withForeignPtr ff \pf ->
	c_hm_field_put_human pf x y >>= \case
		PutHumanResultSuccess -> pure ()
		PutHumanResultPartial -> throw PutHumanPartialError
		PutHumanResultOffscreen -> throw PutHumanOffscreenError
		PutHumanResult n -> throw $ PutHumanUnknownError n

newtype Image = Image (ForeignPtr Image) deriving Show

foreign import ccall "hm_field_get_image"
	c_hm_field_get_image :: Ptr (Field s) -> IO (Ptr Image)

foreign import ccall "hm_image_destroy" c_hm_image_destroy :: Ptr Image -> IO ()

fieldGetImageRaw :: Field s -> IO Image
fieldGetImageRaw (Field ff) = Image <$> withForeignPtr ff \pf -> do
	p <- c_hm_field_get_image pf
	newForeignPtr p $ c_hm_image_destroy p

foreign import ccall "hm_image_draw" c_hm_image_draw :: Ptr Image -> IO ()

imageDraw :: Image -> IO ()
imageDraw (Image fi) = withForeignPtr fi c_hm_image_draw

fieldNewSt :: ST s (Field s)
fieldNewSt = unsafeIOToST fieldNewRaw

fieldPutHumanSt :: Field s -> CInt -> CInt -> ST s ()
fieldPutHumanSt f x y = unsafeIOToST $ fieldPutHumanRaw f x y

fieldGetImageSt :: Field s -> ST s Image
fieldGetImageSt f = unsafeIOToST $ fieldGetImageRaw f
