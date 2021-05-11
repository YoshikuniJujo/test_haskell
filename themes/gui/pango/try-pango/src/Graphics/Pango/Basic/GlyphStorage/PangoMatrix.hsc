{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.GlyphStorage.PangoMatrix where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.Types
import Control.Monad.Primitive
import System.IO.Unsafe

#include <pango/pango.h>

newtype PangoMatrix = PangoMatrix_ (ForeignPtr PangoMatrix)

instance Show PangoMatrix where
	showsPrec d m = showParen (d > 10) $ ("PangoMatrix {" ++) .
		("pangoMatrixXx = " ++) . showsPrec 11 xx . (", " ++) .
		("pangoMatrixXy = " ++) . showsPrec 11 xy . (", " ++) .
		("pangoMatrixYx = " ++) . showsPrec 11 yx . (", " ++) .
		("pangoMatrixYy = " ++) . showsPrec 11 yy . (", " ++) .
		("pangoMatrixX0 = " ++) . showsPrec 11 x0 . (", " ++) .
		("pangoMatrixY0 = " ++) . showsPrec 11 y0 . ("}" ++)
		where PangoMatrix xx xy yx yy x0 y0 = m

pattern PangoMatrix ::
	CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble ->
	PangoMatrix
pattern PangoMatrix {
	pangoMatrixXx, pangoMatrixXy, pangoMatrixYx, pangoMatrixYy,
	pangoMatrixX0, pangoMatrixY0 } <- (pangoMatrix -> (
	pangoMatrixXx, pangoMatrixXy, pangoMatrixYx, pangoMatrixYy,
	pangoMatrixX0, pangoMatrixY0 )) where
	PangoMatrix xx xy yx yy x0 y0 = unsafePerformIO $ PangoMatrix_ <$> do
		p <- mallocBytes #{size PangoMatrix}
		#{poke PangoMatrix, xx} p xx
		#{poke PangoMatrix, xy} p xy
		#{poke PangoMatrix, yx} p yx
		#{poke PangoMatrix, yy} p yy
		#{poke PangoMatrix, x0} p x0
		#{poke PangoMatrix, y0} p y0
		newForeignPtr p (free p)

pangoMatrix :: PangoMatrix -> (CDouble, CDouble, CDouble, CDouble, CDouble, CDouble)
pangoMatrix (PangoMatrix_ fm) = unsafePerformIO $ withForeignPtr fm \pm -> (,,,,,)
	<$> #{peek PangoMatrix, xx} pm
	<*> #{peek PangoMatrix, xy} pm
	<*> #{peek PangoMatrix, yx} pm
	<*> #{peek PangoMatrix, yy} pm
	<*> #{peek PangoMatrix, x0} pm
	<*> #{peek PangoMatrix, y0} pm

newtype PangoMatrixPrim s = PangoMatrixPrim (ForeignPtr (PangoMatrixPrim s)) deriving Show

pangoMatrixFreeze ::
	PrimMonad m => PangoMatrixPrim (PrimState m) -> m PangoMatrix
pangoMatrixFreeze (PangoMatrixPrim fm) = unsafeIOToPrim . (PangoMatrix_ <$>)
	$ withForeignPtr fm c_pango_matrix_freeze >>=
		newForeignPtr <$> id <*> c_pango_matrix_free

foreign import ccall "pango_matrix_copy" c_pango_matrix_freeze ::
	Ptr (PangoMatrixPrim s) -> IO (Ptr PangoMatrix)

foreign import ccall "pango_matrix_free" c_pango_matrix_free ::
	Ptr PangoMatrix -> IO ()

pangoMatrixThaw ::
	PrimMonad m => PangoMatrix -> m (PangoMatrixPrim (PrimState m))
pangoMatrixThaw (PangoMatrix_ fm) = unsafeIOToPrim . (PangoMatrixPrim <$>)
	$ withForeignPtr fm c_pango_matrix_thaw >>=
		newForeignPtr <$> id <*> c_pango_matrix_prim_free

foreign import ccall "pango_matrix_copy" c_pango_matrix_thaw ::
	Ptr PangoMatrix -> IO (Ptr (PangoMatrixPrim s))

pangoMatrixCopy :: PrimMonad m =>
	PangoMatrixPrim (PrimState m) -> m (PangoMatrixPrim (PrimState m))
pangoMatrixCopy (PangoMatrixPrim fm) = unsafeIOToPrim . (PangoMatrixPrim <$>)
	$ withForeignPtr fm c_pango_matrix_copy >>=
		newForeignPtr <$> id <*> c_pango_matrix_prim_free

foreign import ccall "pango_matrix_copy" c_pango_matrix_copy ::
	Ptr (PangoMatrixPrim s) -> IO (Ptr (PangoMatrixPrim s))

foreign import ccall "pango_matrix_free" c_pango_matrix_prim_free ::
	Ptr (PangoMatrixPrim s) -> IO ()

pangoMatrixTranslate :: PrimMonad m =>
	PangoMatrixPrim (PrimState m) -> CDouble -> CDouble -> m ()
pangoMatrixTranslate (PangoMatrixPrim fm) tx ty = unsafeIOToPrim
	$ withForeignPtr fm \pm -> c_pango_matrix_translate pm tx ty

foreign import ccall "pango_matrix_translate" c_pango_matrix_translate ::
	Ptr (PangoMatrixPrim s) -> CDouble -> CDouble -> IO ()

pangoMatrixScale :: PrimMonad m =>
	PangoMatrixPrim (PrimState m) -> CDouble -> CDouble -> m ()
pangoMatrixScale (PangoMatrixPrim fm) sx sy = unsafeIOToPrim
	$ withForeignPtr fm \pm -> c_pango_matrix_scale pm sx sy

foreign import ccall "pango_matrix_scale" c_pango_matrix_scale ::
	Ptr (PangoMatrixPrim s) -> CDouble -> CDouble -> IO ()

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
	Ptr (PangoMatrixPrim s) -> CDouble -> IO ()

pangoMatrixConcat :: PrimMonad m =>
	PangoMatrixPrim (PrimState m) -> PangoMatrix -> m ()
pangoMatrixConcat (PangoMatrixPrim fm) (PangoMatrix_ fnm) = unsafeIOToPrim
	$ withForeignPtr fm \pm -> withForeignPtr fnm \pnm ->
		c_pango_matrix_concat pm pnm

foreign import ccall "pango_matrix_concat" c_pango_matrix_concat ::
	Ptr (PangoMatrixPrim s) -> Ptr PangoMatrix -> IO ()

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
