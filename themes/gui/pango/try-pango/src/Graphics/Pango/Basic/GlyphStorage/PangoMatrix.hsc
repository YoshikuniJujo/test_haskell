{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.GlyphStorage.PangoMatrix where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.Types
import Control.Monad.Primitive
import System.IO.Unsafe

import Foreign.C.Struct

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

data Angle = Radian_ CDouble | Degree_ CDouble deriving Show

{-# COMPLETE Radian #-}

pattern Radian :: CDouble -> Angle
pattern Radian r <- (radian -> r) where Radian = Radian_

radian :: Angle -> CDouble
radian = \case Radian_ r -> r; Degree_ d -> d / 360 * 2 * pi

{-# COMPLETE Degree #-}

pattern Degree :: CDouble -> Angle
pattern Degree d <- (degree -> d) where Degree = Degree_

degree :: Angle -> CDouble
degree = \case Radian_ r -> r / (2 * pi) * 360; Degree_ d -> d

pangoMatrixRotate :: PrimMonad m =>
	PangoMatrixPrim (PrimState m) -> Angle -> m ()
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
