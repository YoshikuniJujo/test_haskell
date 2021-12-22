{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Human where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String
import Foreign.C.Enum
import Foreign.C.Struct
import Control.Monad.Primitive
import Control.Monad.ST
import Control.Monad.ST.Unsafe
import Control.Exception
import Data.Word

import Human.Exception

#include <human.h>
#include <stdbool.h>

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

fieldNew :: PrimMonad m => m (Field (PrimState m))
fieldNew = unsafeIOToPrim fieldNewRaw

fieldClear :: PrimMonad m => Field (PrimState m) -> m ()
fieldClear = unsafeIOToPrim . fieldClearRaw

fieldPutHuman :: PrimMonad m => Field (PrimState m) -> CInt -> CInt -> m ()
fieldPutHuman f x y = unsafeIOToPrim $ fieldPutHumanRaw f x y

fieldGetImage :: PrimMonad m => Field (PrimState m) -> m Image
fieldGetImage = unsafeIOToPrim . fieldGetImageRaw

fieldDraw :: Field RealWorld -> IO ()
fieldDraw = fieldDrawRaw

enum "Head" ''#{type HmHead} [''Show, ''Read, ''Storable] [
	("SmallHead", #{const HM_SMALL_HEAD}),
	("LargeHead", #{const HM_LARGE_HEAD}) ]

enum "Arm" ''#{type HmArm} [''Show, ''Read, ''Storable] [
	("DownArm", #{const HM_DOWN_ARM}), ("UpArm", #{const HM_UP_ARM}) ]

struct "Human" #{size HmHuman}
	[	("headSize", ''Head, [| #{peek HmHuman, head_size} |],
			[| #{poke HmHuman, head_size} |]),
		("leftArm", ''Arm, [| #{peek HmHuman, left_arm} |],
			[| #{poke HmHuman, left_arm} |]),
		("rightArm", ''Arm, [| #{peek HmHuman, right_arm} |],
			[| #{poke HmHuman, right_arm} |]) ]
	[''Show, ''Read]

foreign import ccall "hm_field_put_various_human"
	c_hm_field_put_various_human ::
	Ptr (Field s) -> Ptr Human -> CInt -> CInt -> IO PutHumanResult

fieldPutVariousHumanRaw :: Field s -> Human -> CInt -> CInt -> IO ()
fieldPutVariousHumanRaw (Field ff) (Human_ fhm) x y =
	withForeignPtr ff \pf -> withForeignPtr fhm \phm ->
		c_hm_field_put_various_human pf phm x y >>= \case
			PutHumanResultSuccess -> pure ()
			PutHumanResultPartial -> throw PutHumanPartialError
			PutHumanResultOffscreen -> throw PutHumanOffscreenError
			PutHumanResult n -> throw $ PutHumanUnknownError n

fieldPutVariousHuman ::
	PrimMonad m => Field (PrimState m) -> Human -> CInt -> CInt -> m ()
fieldPutVariousHuman f hm x y =
	unsafeIOToPrim $ fieldPutVariousHumanRaw f hm x y

foreign import ccall "hm_human_copy"
	c_hm_human_copy :: Ptr Human -> IO (Ptr Human)

foreign import ccall "hm_human_destroy" c_hm_human_destroy :: Ptr Human -> IO ()

structPrim "Human" 'c_hm_human_copy 'c_hm_human_destroy [''Show]

foreign import ccall "hm_human_flip_head"
	c_hm_human_flip_head :: Ptr Human -> IO ()

foreign import ccall "hm_human_flip_left_arm"
	c_hm_human_flip_left_arm :: Ptr Human -> IO ()

foreign import ccall "hm_human_flip_right_arm"
	c_hm_human_flip_right_arm :: Ptr Human -> IO ()

humanFlipHead, humanFlipLeftArm, humanFlipRightArm ::
	PrimMonad m => HumanPrim (PrimState m) -> m ()
humanFlipHead (HumanPrim fhm) =
	unsafeIOToPrim $ withForeignPtr fhm c_hm_human_flip_head

humanFlipLeftArm (HumanPrim fhm) =
	unsafeIOToPrim $ withForeignPtr fhm c_hm_human_flip_left_arm

humanFlipRightArm (HumanPrim fhm) =
	unsafeIOToPrim $ withForeignPtr fhm c_hm_human_flip_right_arm

foreign import ccall "hm_field_new_background"
	c_hm_field_new_background :: #{type bool} -> FunPtr (IO (Ptr (Field s)))

foreign import ccall "hm_field_clear_background"
	c_hm_field_clear_background :: #{type bool} -> FunPtr (Ptr (Field s) -> IO ())

foreign import ccall "dynamic"
	mkFieldNewBg :: FunPtr (IO (Ptr (Field s))) -> IO (Ptr (Field s))

foreign import ccall "dynamic" mkFieldClearBg ::
	FunPtr (Ptr (Field s) -> IO ()) -> Ptr (Field s) -> IO ()

fieldNewBackgroundRaw :: Bool -> IO (Field s)
fieldNewBackgroundRaw (boolToCBool -> b) = Field <$> do
	pf <- mkFieldNewBg $ c_hm_field_new_background b
	newForeignPtr pf $ c_hm_field_destroy pf

fieldClearBackgroundRaw :: Bool -> Field s -> IO ()
fieldClearBackgroundRaw (boolToCBool -> b) (Field ff) =
	withForeignPtr ff $ mkFieldClearBg (c_hm_field_clear_background b)

boolToCBool :: Bool -> #{type bool}
boolToCBool = \case False -> #{const false}; True -> #{const true}

fieldNewBackground :: PrimMonad m => Bool -> m (Field (PrimState m))
fieldNewBackground = unsafeIOToPrim . fieldNewBackgroundRaw

fieldClearBackground :: PrimMonad m => Bool -> Field (PrimState m) -> m ()
fieldClearBackground b = unsafeIOToPrim . fieldClearBackgroundRaw b

struct "Position" #{size HmPosition}
	[	("x", ''CInt, [| #{peek HmPosition, x} |],
			[| #{poke HmPosition, x} |]),
		("y", ''CInt, [| #{peek HmPosition, y} |],
			[| #{poke HmPosition, y} |]) ]
	[''Show]

type PtrPosition = Ptr Position

struct "CMessage" #{size HmMessage}
	[	("position", ''PtrPosition, [| #{peek HmMessage, position} |],
			[| #{poke HmMessage, position} |]),
		("message", ''CString, [| #{peek HmMessage, message} |],
			[| #{poke HmMessage, message} |]) ]
	[''Show]

data Message = Message { messagePosition :: Position, messageMessage :: String }
	deriving Show

foreign import ccall "hm_field_put_message"
	c_hm_field_put_message :: Ptr (Field s) -> Ptr CMessage -> IO ()

fieldPutMessageRaw :: Field s -> Message -> IO ()
fieldPutMessageRaw (Field ff)
	Message { messagePosition = Position_ fp, messageMessage = msg } =
	withForeignPtr fp \pp -> withCString msg \cmsg -> do
		let	CMessage_ fmsg = CMessage {
				cMessagePosition = pp,
				cMessageMessage = cmsg }
		withForeignPtr ff $ withForeignPtr fmsg . c_hm_field_put_message

fieldPutMessage :: PrimMonad m => Field (PrimState m) -> Message -> m ()
fieldPutMessage f = unsafeIOToPrim . fieldPutMessageRaw f
