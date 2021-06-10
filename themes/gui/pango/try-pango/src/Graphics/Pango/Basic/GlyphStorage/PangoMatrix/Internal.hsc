{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.GlyphStorage.PangoMatrix.Internal (

	-- * TYPE
	-- ** PangoMatrix
	PangoMatrix(..), pattern PangoMatrix,
	pangoMatrixXx, pangoMatrixXy, pangoMatrixYx, pangoMatrixYy,
	pangoMatrixX0, pangoMatrixY0,

	-- ** PangoMatrixPrim
	PangoMatrixPrim, PangoMatrixST, PangoMatrixIO,
	pangoMatrixFreeze, pangoMatrixThaw, pangoMatrixCopy,

	-- ** PangoMatrixNullable
	PangoMatrixNullable(..), pangoMatrixFromNullable, pangoMatrixToNullable,
	mkPangoMatrixNullable0,

	-- * FUNCTION
	pangoMatrixTranslate, pangoMatrixScale, pangoMatrixRotate,
	pangoMatrixConcat,

	pangoMatrixTransformPoint, pangoMatrixTransformDistance,
	pangoMatrixTransformRectangle, pangoMatrixTransformPixelRectangle,
	pangoMatrixGetFontScaleFactor, pangoMatrixGetFontScaleFactors

	) where

import Foreign.Ptr
import Foreign.Ptr.Misc
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.Types
import Control.Monad.Primitive
import Data.Angle
import System.IO.Unsafe

import Foreign.C.Struct

import Graphics.Pango.Basic.GlyphStorage.Internal

#include <pango/pango.h>

struct "PangoMatrix" #{size PangoMatrix}
	[	("xx", ''CDouble, [| #{peek PangoMatrix, xx} |],
			[| #{poke PangoMatrix, xx} |]),
		("xy", ''CDouble, [| #{peek PangoMatrix, xy} |],
			[| #{poke PangoMatrix, xy} |]),
		("yx", ''CDouble, [| #{peek PangoMatrix, yx} |],
			[| #{poke PangoMatrix, yx} |]),
		("yy", ''CDouble, [| #{peek PangoMatrix, yy} |],
			[| #{poke PangoMatrix, yy} |]),
		("x0", ''CDouble, [| #{peek PangoMatrix, x0} |],
			[| #{poke PangoMatrix, x0} |]),
		("y0", ''CDouble, [| #{peek PangoMatrix, y0} |],
			[| #{poke PangoMatrix, y0} |]) ]
	[''Show]

foreign import ccall "pango_matrix_copy" c_pango_matrix_copy ::
	Ptr PangoMatrix -> IO (Ptr PangoMatrix)

foreign import ccall "pango_matrix_free" c_pango_matrix_free ::
	Ptr PangoMatrix -> IO ()

structPrim "PangoMatrix" 'c_pango_matrix_copy 'c_pango_matrix_free [''Show]

pangoMatrixTranslate :: PrimMonad m =>
	PangoMatrixPrim (PrimState m) -> CDouble -> CDouble -> m ()
pangoMatrixTranslate (PangoMatrixPrim fm) tx ty = unsafeIOToPrim
	$ withForeignPtr fm \pm -> c_pango_matrix_translate pm tx ty

foreign import ccall "pango_matrix_translate" c_pango_matrix_translate ::
	Ptr PangoMatrix -> CDouble -> CDouble -> IO ()

pangoMatrixScale :: PrimMonad m =>
	PangoMatrixPrim (PrimState m) -> CDouble -> CDouble -> m ()
pangoMatrixScale (PangoMatrixPrim fm) sx sy = unsafeIOToPrim
	$ withForeignPtr fm \pm -> c_pango_matrix_scale pm sx sy

foreign import ccall "pango_matrix_scale" c_pango_matrix_scale ::
	Ptr PangoMatrix -> CDouble -> CDouble -> IO ()

pangoMatrixRotate :: PrimMonad m =>
	PangoMatrixPrim (PrimState m) -> Angle CDouble -> m ()
pangoMatrixRotate (PangoMatrixPrim fm) (Degree dgr) = unsafeIOToPrim
	$ withForeignPtr fm \pm -> c_pango_matrix_rotate pm dgr

foreign import ccall "pango_matrix_rotate" c_pango_matrix_rotate ::
	Ptr PangoMatrix -> CDouble -> IO ()

pangoMatrixConcat :: PrimMonad m =>
	PangoMatrixPrim (PrimState m) -> PangoMatrix -> m ()
pangoMatrixConcat (PangoMatrixPrim fm) (PangoMatrix_ fnm) = unsafeIOToPrim
	$ withForeignPtr fm \pm -> withForeignPtr fnm \pnm ->
		c_pango_matrix_concat pm pnm

foreign import ccall "pango_matrix_concat" c_pango_matrix_concat ::
	Ptr PangoMatrix -> Ptr PangoMatrix -> IO ()

pangoMatrixTransformPoint ::
	PangoMatrix -> CDouble -> CDouble -> (CDouble, CDouble)
pangoMatrixTransformPoint (PangoMatrix_ fm) x y = unsafePerformIO
	$ withForeignPtr fm \pm -> alloca \px -> alloca \py -> do
		poke px x; poke py y
		c_pango_matrix_transform_point pm px py
		(,) <$> peek px <*> peek py

foreign import ccall "pango_matrix_transform_point"
	c_pango_matrix_transform_point ::
	Ptr PangoMatrix -> Ptr CDouble -> Ptr CDouble -> IO ()

pangoMatrixTransformDistance ::
	PangoMatrix -> CDouble -> CDouble -> (CDouble, CDouble)
pangoMatrixTransformDistance (PangoMatrix_ fm) dx dy = unsafePerformIO
	$ withForeignPtr fm \pm -> alloca \pdx -> alloca \pdy -> do
		poke pdx dx; poke pdy dy
		c_pango_matrix_transform_distance pm pdx pdy
		(,) <$> peek pdx <*> peek pdy

foreign import ccall "pango_matrix_transform_distance"
	c_pango_matrix_transform_distance ::
	Ptr PangoMatrix -> Ptr CDouble -> Ptr CDouble -> IO ()

pangoMatrixTransformRectangle ::
	PrimMonad m => PangoMatrix -> PangoRectangleFixedPrim (PrimState m) -> m ()
pangoMatrixTransformRectangle (PangoMatrix_ fm) (PangoRectangleFixedPrim fr) =
	unsafeIOToPrim $ withForeignPtr fm \pm -> withForeignPtr fr \pr ->
		c_pango_matrix_transform_rectangle pm pr

foreign import ccall "pango_matrix_transform_rectangle"
	c_pango_matrix_transform_rectangle ::
	Ptr PangoMatrix -> Ptr PangoRectangleFixed -> IO ()

pangoMatrixTransformPixelRectangle :: PrimMonad m =>
	PangoMatrix -> PangoRectanglePixelPrim (PrimState m) -> m ()
pangoMatrixTransformPixelRectangle
	(PangoMatrix_ fm) (PangoRectanglePixelPrim fr) =
	unsafeIOToPrim $ withForeignPtr fm \pm -> withForeignPtr fr \pr ->
		c_pango_matrix_transform_pixel_rectangle pm pr

foreign import ccall "pango_matrix_transform_pixel_rectangle"
	c_pango_matrix_transform_pixel_rectangle ::
	Ptr PangoMatrix -> Ptr PangoRectanglePixel -> IO ()

pangoMatrixGetFontScaleFactor :: PangoMatrix -> CDouble
pangoMatrixGetFontScaleFactor (PangoMatrix_ fm) = unsafePerformIO
	$ withForeignPtr fm c_pango_matrix_get_font_scale_factor

foreign import ccall "pango_matrix_get_font_scale_factor"
	c_pango_matrix_get_font_scale_factor ::
	Ptr PangoMatrix -> IO CDouble

pangoMatrixGetFontScaleFactors :: PangoMatrix -> (CDouble, CDouble)
pangoMatrixGetFontScaleFactors (PangoMatrix_ fm) = unsafePerformIO
	$ withForeignPtr fm \pm -> alloca \xs -> alloca \ys -> do
		c_pango_matrix_get_font_scale_factors pm xs ys
		(,) <$> peek xs <*> peek ys

foreign import ccall "pango_matrix_get_font_scale_factors"
	c_pango_matrix_get_font_scale_factors ::
	Ptr PangoMatrix -> Ptr CDouble -> Ptr CDouble -> IO ()

data PangoMatrixNullable
	= PangoMatrixNull
	| PangoMatrixNotNull (ForeignPtr PangoMatrix)
	deriving Show

pangoMatrixToNullable :: Maybe PangoMatrix -> PangoMatrixNullable
pangoMatrixToNullable = \case
	Nothing -> PangoMatrixNull
	Just (PangoMatrix_ f) -> PangoMatrixNotNull f

pangoMatrixFromNullable :: PangoMatrixNullable -> Maybe PangoMatrix
pangoMatrixFromNullable = \case
	PangoMatrixNull -> Nothing
	PangoMatrixNotNull f -> Just $ PangoMatrix_ f

mkPangoMatrixNullable0 :: Ptr PangoMatrix -> IO PangoMatrixNullable
mkPangoMatrixNullable0 = \case
	NullPtr -> pure PangoMatrixNull
	pm -> PangoMatrixNotNull <$> do
		pm' <- c_pango_matrix_copy pm
		newForeignPtr pm' $ c_pango_matrix_free pm'
