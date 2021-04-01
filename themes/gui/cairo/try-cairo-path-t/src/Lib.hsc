{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Storable
import Control.Monad.Primitive
import Data.Word
import Data.CairoContext

import Graphics.Cairo.Exception

#include <cairo.h>

newtype CairoPathT = CairoPathT (ForeignPtr CairoPathT) deriving Show

cairoCopyPath :: PrimMonad m => CairoT (PrimState m) -> m CairoPathT
cairoCopyPath (CairoT fcr) = unsafeIOToPrim
	$ CairoPathT <$> withForeignPtr fcr \pcr -> do
		p <- c_cairo_copy_path pcr
		newForeignPtr p (c_cairo_path_destroy p) <*
			(cairoStatusToThrowError =<< cairoPathStatus p)

foreign import ccall "cairo_copy_path" c_cairo_copy_path ::
	Ptr (CairoT s) -> IO (Ptr CairoPathT)

foreign import ccall "cairo_path_destroy" c_cairo_path_destroy ::
	Ptr CairoPathT -> IO ()

cairoPathStatus :: Ptr CairoPathT -> IO #{type cairo_status_t}
cairoPathStatus = #{peek cairo_path_t, status}
