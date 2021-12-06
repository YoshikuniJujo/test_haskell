{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Human where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.Enum
import Foreign.C.Struct
import Control.Monad.Primitive
import Control.Monad.ST
import Control.Monad.ST.Unsafe
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

fieldNewRaw :: IO (Field s)
fieldNewRaw = Field <$> do
	p <- c_hm_field_new
	newForeignPtr p $ c_hm_field_destroy p

fieldNewSt :: ST s (Field s)
fieldNewSt = unsafeIOToST fieldNewRaw

fieldNew :: PrimMonad m => m (Field (PrimState m))
fieldNew = unsafeIOToPrim fieldNewRaw

fieldClear :: PrimMonad m => Field (PrimState m) -> m ()
fieldClear (Field ff) = unsafeIOToPrim
	$ withForeignPtr ff c_hm_field_clear

fieldDraw :: Field s -> IO ()
fieldDraw (Field ff) = withForeignPtr ff c_hm_field_draw

foreign import ccall "hm_field_new" c_hm_field_new :: IO (Ptr (Field s))
foreign import ccall "hm_field_clear" c_hm_field_clear :: Ptr (Field s) -> IO ()
foreign import ccall "hm_field_draw" c_hm_field_draw :: Ptr (Field s) -> IO ()
foreign import ccall "hm_field_destroy" c_hm_field_destroy :: Ptr (Field s) -> IO ()

fieldPutHumanRaw :: Field s -> CInt -> CInt -> IO ()
fieldPutHumanRaw (Field ff) x y = withForeignPtr ff \pf -> do
	r <- c_hm_field_put_human pf x y
	case r of
		PutHumanResultSuccess -> pure ()
		PutHumanResultPartial -> throw PutHumanPartialError
		PutHumanResultOffscreen -> throw PutHumanOffscreenError
		PutHumanResult n -> throw $ PutHumanUnknownError n

fieldPutHumanSt :: Field s -> CInt -> CInt -> ST s ()
fieldPutHumanSt f x y = unsafeIOToST $ fieldPutHumanRaw f x y

fieldPutHuman :: PrimMonad m => Field (PrimState m) -> CInt -> CInt -> m ()
fieldPutHuman f x y = unsafeIOToPrim $ fieldPutHumanRaw f x y

foreign import ccall "hm_field_put_human"
	c_hm_field_put_human :: Ptr (Field s) -> CInt -> CInt -> IO PutHumanResult

newtype Image = Image (ForeignPtr Image) deriving Show

fieldGetImageRaw :: Field s -> IO Image
fieldGetImageRaw (Field ff) = Image <$> withForeignPtr ff \pf -> do
	pimg <- c_hm_field_get_image pf
	newForeignPtr pimg $ c_hm_image_destroy pimg

fieldGetImageSt :: Field s -> ST s Image
fieldGetImageSt f = unsafeIOToST $ fieldGetImageRaw f

fieldGetImage :: PrimMonad m => Field (PrimState m) -> m Image
fieldGetImage f = unsafeIOToPrim $ fieldGetImageRaw f

foreign import ccall "hm_field_get_image"
	c_hm_field_get_image :: Ptr (Field s) -> IO (Ptr Image)

foreign import ccall "hm_image_destroy" c_hm_image_destroy :: Ptr Image -> IO ()

imageDraw :: Image -> IO ()
imageDraw (Image fimg) = withForeignPtr fimg c_hm_image_draw

foreign import ccall "hm_image_draw" c_hm_image_draw :: Ptr Image -> IO ()

enum "Head" ''#{type HmHead} [''Show, ''Read, ''Storable] [
	("SmallHead", #{const HM_SMALL_HEAD}),
	("LargeHead", #{const HM_LARGE_HEAD}) ]

enum "Arm" ''#{type HmArm} [''Show, ''Read, ''Storable] [
	("DownArm", #{const HM_DOWN_ARM}),
	("UpArm", #{const HM_UP_ARM}) ]

struct "Human" #{size HmHuman}
	[	("headSize", ''Head,
			[| #{peek HmHuman, head_size} |],
			[| #{poke HmHuman, head_size} |]),
		("leftArm", ''Arm,
			[| #{peek HmHuman, left_arm} |],
			[| #{poke HmHuman, left_arm} |]),
		("rightArm", ''Arm,
			[| #{peek HmHuman, right_arm} |],
			[| #{poke HmHuman, right_arm} |]) ]
	[''Show, ''Read]

fieldPutVariousHumanRaw :: Field s -> Human -> CInt -> CInt -> IO ()
fieldPutVariousHumanRaw (Field ff) (Human_ fhm) x y =
	withForeignPtr ff \pf -> withForeignPtr fhm \phm -> do
		r <- c_hm_field_put_various_human pf phm x y
		case r of
			PutHumanResultSuccess -> pure ()
			PutHumanResultPartial -> throw PutHumanPartialError
			PutHumanResultOffscreen -> throw PutHumanOffscreenError
			PutHumanResult n -> throw $ PutHumanUnknownError n

fieldPutVariousHuman ::
	PrimMonad m => Field (PrimState m) -> Human -> CInt -> CInt -> m ()
fieldPutVariousHuman f hm x y =
	unsafeIOToPrim $ fieldPutVariousHumanRaw f hm x y

foreign import ccall "hm_field_put_various_human"
	c_hm_field_put_various_human ::
	Ptr (Field s) -> Ptr Human -> CInt -> CInt -> IO PutHumanResult

foreign import ccall "hm_human_copy"
	c_hm_human_copy :: Ptr Human -> IO (Ptr Human)

foreign import ccall "hm_human_destroy" c_hm_human_destroy :: Ptr Human -> IO ()

structPrim "Human" 'c_hm_human_copy 'c_hm_human_destroy [''Show]

humanFlipHead, humanFlipLeftArm, humanFlipRightArm ::
	PrimMonad m => HumanPrim (PrimState m) -> m ()
humanFlipHead (HumanPrim fhm) =
	unsafeIOToPrim $ withForeignPtr fhm c_hm_human_flip_head

humanFlipLeftArm (HumanPrim fhm) =
	unsafeIOToPrim $ withForeignPtr fhm c_hm_human_flip_left_arm

humanFlipRightArm (HumanPrim fhm) =
	unsafeIOToPrim $ withForeignPtr fhm c_hm_human_flip_right_arm

foreign import ccall "hm_human_flip_head"
	c_hm_human_flip_head :: Ptr Human -> IO ()
foreign import ccall "hm_human_flip_left_arm"
	c_hm_human_flip_left_arm :: Ptr Human -> IO ()
foreign import ccall "hm_human_flip_right_arm"
	c_hm_human_flip_right_arm :: Ptr Human -> IO ()
