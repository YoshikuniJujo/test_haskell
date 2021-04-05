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

cairoMatrixAlloca :: PrimMonad m =>
	(Ptr (CairoMatrixT (PrimState m)) -> IO a) ->
	m (ForeignPtr (CairoMatrixT (PrimState m)))
cairoMatrixAlloca f = unsafeIOToPrim do
	p <- mallocBytes #{size cairo_matrix_t}
	(($) <$> newForeignPtr <*> free $ p) <* f p

data Matrix = Matrix CDouble CDouble CDouble CDouble CDouble CDouble deriving Show

cairoMatrixGet :: (PrimMonad m, IsCairoMatrixT mtx) => mtx (PrimState m) -> m Matrix
cairoMatrixGet mtx = withCairoMatrixT mtx \p -> Matrix
	<$> #{peek cairo_matrix_t, xx} p <*> #{peek cairo_matrix_t, yx} p
	<*> #{peek cairo_matrix_t, xy} p <*> #{peek cairo_matrix_t, yy} p
	<*> #{peek cairo_matrix_t, x0} p <*> #{peek cairo_matrix_t, y0} p

cairoMatrixNew :: PrimMonad m =>
	CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> 
	m (CairoMatrixT (PrimState m))
cairoMatrixNew xx yx xy yy x0 y0 = CairoMatrixT
	<$> cairoMatrixAlloca \p -> c_cairo_matrix_init p xx yx xy yy x0 y0

cairoMatrixRegularNew :: PrimMonad m =>
	CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> 
	m (Either (CairoMatrixT (PrimState m)) (CairoMatrixRegularT (PrimState m)))
cairoMatrixRegularNew xx yx xy yy x0 y0 = mk
	<$> cairoMatrixAlloca \p -> c_cairo_matrix_init p xx yx xy yy x0 y0
	where mk = case xx * yy - yx * xy of
		0 -> Left . CairoMatrixT; _ -> Right . CairoMatrixRegularT

foreign import ccall "cairo_matrix_init" c_cairo_matrix_init ::
	Ptr (CairoMatrixT s) -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> IO ()

cairoMatrixNewIdentity :: PrimMonad m => m (CairoMatrixRegularT (PrimState m))
cairoMatrixNewIdentity = CairoMatrixRegularT
	<$> cairoMatrixAlloca c_cairo_matrix_init_identity

foreign import ccall "cairo_matrix_init_identity" c_cairo_matrix_init_identity ::
	Ptr (CairoMatrixT s) -> IO ()

cairoMatrixNewTranslate :: PrimMonad m =>
	CDouble -> CDouble -> m (CairoMatrixRegularT (PrimState m))
cairoMatrixNewTranslate tx ty = CairoMatrixRegularT
	<$> cairoMatrixAlloca \p -> c_cairo_matrix_init_translate p tx ty

foreign import ccall "cairo_matrix_init_translate" c_cairo_matrix_init_translate ::
	Ptr (CairoMatrixT s) -> CDouble -> CDouble -> IO ()

cairoMatrixNewScale :: PrimMonad m =>
	CDouble -> CDouble -> m (CairoMatrixT (PrimState m))
cairoMatrixNewScale sx sy = CairoMatrixT
	<$> cairoMatrixAlloca \p -> c_cairo_matrix_init_scale p sx sy

cairoMatrixRegularNewScale :: PrimMonad m =>
	CDouble -> CDouble ->
	m (Either (CairoMatrixT (PrimState m)) (CairoMatrixRegularT (PrimState m)))
cairoMatrixRegularNewScale sx sy = mk
	<$> cairoMatrixAlloca \p -> c_cairo_matrix_init_scale p sx sy
	where mk = case (sx, sy) of
		(_, 0) -> Left . CairoMatrixT
		(0, _) -> Left . CairoMatrixT
		_ -> Right . CairoMatrixRegularT

foreign import ccall "cairo_matrix_init_scale" c_cairo_matrix_init_scale ::
	Ptr (CairoMatrixT s) -> CDouble -> CDouble -> IO ()

cairoMatrixNewRotate :: PrimMonad m =>
	CDouble -> m (CairoMatrixRegularT (PrimState m))
cairoMatrixNewRotate rad = CairoMatrixRegularT
	<$> cairoMatrixAlloca \p -> c_cairo_matrix_init_rotate p rad

foreign import ccall "cairo_matrix_init_rotate" c_cairo_matrix_init_rotate ::
	Ptr (CairoMatrixT s) -> CDouble -> IO ()

cairoMatrixTranslate :: (PrimMonad m, IsCairoMatrixT mtx) =>
	mtx (PrimState m) -> CDouble -> CDouble -> m ()
cairoMatrixTranslate mtx tx ty =
	withCairoMatrixT mtx \p -> c_cairo_matrix_translate p tx ty

foreign import ccall "cairo_matrix_translate" c_cairo_matrix_translate ::
	Ptr (CairoMatrixT s) -> CDouble -> CDouble -> IO ()

cairoMatrixRotate :: (PrimMonad m, IsCairoMatrixT mtx) =>
	mtx (PrimState m) -> CDouble -> m ()
cairoMatrixRotate mtx rad =
	withCairoMatrixT mtx \p -> c_cairo_matrix_rotate p rad

foreign import ccall "cairo_matrix_rotate" c_cairo_matrix_rotate ::
	Ptr (CairoMatrixT s) -> CDouble -> IO ()
