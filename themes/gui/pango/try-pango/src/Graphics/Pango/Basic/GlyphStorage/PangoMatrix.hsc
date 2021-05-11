{-# LANGUAGE BlockArguments #-}
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

foreign import ccall "pango_matrix_freeze" c_pango_matrix_freeze ::
	Ptr (PangoMatrixPrim s) -> IO (Ptr PangoMatrix)

foreign import ccall "pango_matrix_copy" c_pango_matrix_copy ::
	Ptr (PangoMatrixPrim s) -> IO (Ptr (PangoMatrixPrim s))

foreign import ccall "pango_matrix_free" c_pango_matrix_free ::
	Ptr PangoMatrix -> IO ()

foreign import ccall "pango_matrix_free" c_pango_matrix_prim_free ::
	Ptr (PangoMatrixPrim s) -> IO ()
