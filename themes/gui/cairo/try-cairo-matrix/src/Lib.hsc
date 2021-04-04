{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.Types
import Control.Monad.Primitive

#include <cairo.h>

class IsCairoMatrixT mtx where toCairoMatrixT :: mtx s -> CairoMatrixT s

withCairoMatrixT :: (PrimMonad m, IsCairoMatrixT mtx) =>
	mtx (PrimState m) -> (Ptr (CairoMatrixT (PrimState m)) -> IO a) -> m a
withCairoMatrixT (toCairoMatrixT -> CairoMatrixT fmtx) = unsafeIOToPrim . withForeignPtr fmtx

newtype CairoMatrixT s = CairoMatrixT (ForeignPtr (CairoMatrixT s)) deriving Show

instance IsCairoMatrixT CairoMatrixT where toCairoMatrixT = id

newtype CairoMatrixRegularT s = CairoMatrixRegularT (ForeignPtr (CairoMatrixT s)) deriving Show

instance IsCairoMatrixT CairoMatrixRegularT where toCairoMatrixT (CairoMatrixRegularT f) = CairoMatrixT f

cairoMatrixNew :: PrimMonad m =>
	CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> 
	m (CairoMatrixT (PrimState m))
cairoMatrixNew xx yx xy yy x0 y0 = unsafeIOToPrim $ CairoMatrixT <$> do
	p <- mallocBytes #{size cairo_matrix_t}
	c_cairo_matrix_init p xx yx xy yy x0 y0
	($) <$> newForeignPtr <*> free $ p

cairoMatrixRegularNew :: PrimMonad m =>
	CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> 
	m (Either (CairoMatrixT (PrimState m)) (CairoMatrixRegularT (PrimState m)))
cairoMatrixRegularNew xx yx xy yy x0 y0 = unsafeIOToPrim do
	p <- mallocBytes #{size cairo_matrix_t}
	c_cairo_matrix_init p xx yx xy yy x0 y0
	(case xx * yy - yx * xy of 0 -> Left . CairoMatrixT; _ -> Right . CairoMatrixRegularT)
		<$> (($) <$> newForeignPtr  <*> free) p

data Matrix = Matrix CDouble CDouble CDouble CDouble CDouble CDouble deriving Show

cairoMatrixGet :: (PrimMonad m, IsCairoMatrixT mtx) => mtx (PrimState m) -> m Matrix
cairoMatrixGet mtx = withCairoMatrixT mtx \p -> Matrix
	<$> #{peek cairo_matrix_t, xx} p <*> #{peek cairo_matrix_t, yx} p
	<*> #{peek cairo_matrix_t, xy} p <*> #{peek cairo_matrix_t, yy} p
	<*> #{peek cairo_matrix_t, x0} p <*> #{peek cairo_matrix_t, y0} p

foreign import ccall "cairo_matrix_init" c_cairo_matrix_init ::
	Ptr (CairoMatrixT s) -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> IO ()
