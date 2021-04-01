{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Storable
import Foreign.C.Types
import Control.Monad.Primitive
import Data.Word
import Data.CairoContext

import Graphics.Cairo.Exception

#include <cairo.h>

newtype CairoPathT = CairoPathT (ForeignPtr CairoPathT) deriving Show

withCairoPathT :: CairoPathT -> (Ptr CairoPathT -> IO a) -> IO a
withCairoPathT (CairoPathT fpth) = withForeignPtr fpth

cairoCopyPath :: PrimMonad m => CairoT (PrimState m) -> m CairoPathT
cairoCopyPath (CairoT fcr) = unsafeIOToPrim
	$ CairoPathT <$> withForeignPtr fcr \pcr -> do
		p <- c_cairo_copy_path pcr
		newForeignPtr p (c_cairo_path_destroy p) <*
			(cairoStatusToThrowError =<< cairoPathTStatus p)

foreign import ccall "cairo_copy_path" c_cairo_copy_path ::
	Ptr (CairoT s) -> IO (Ptr CairoPathT)

foreign import ccall "cairo_path_destroy" c_cairo_path_destroy ::
	Ptr CairoPathT -> IO ()

cairoPathTStatus :: Ptr CairoPathT -> IO #{type cairo_status_t}
cairoPathTStatus = #{peek cairo_path_t, status}

cairoPathTData ::Ptr CairoPathT -> IO (Ptr CairoPathDataT)
cairoPathTData = #{peek cairo_path_t, data}

cairoPathTNumData :: Ptr CairoPathT -> IO CInt
cairoPathTNumData = #{peek cairo_path_t, num_data}

newtype CairoPathDataT = CairoPathDataT (Ptr CairoPathDataT) deriving Show

nextPtr :: Ptr a -> Int -> Int -> Ptr a
nextPtr p sz al = alignPtr (plusPtr p sz) al

nextCairoPathDataT :: Ptr CairoPathDataT -> Ptr CairoPathDataT
nextCairoPathDataT p = nextPtr p #{size cairo_path_data_t} #{alignment cairo_path_data_t}

nextByLength :: Ptr CairoPathDataT -> CInt -> Ptr CairoPathDataT
nextByLength p n | n < 1 = p
nextByLength p n = nextCairoPathDataT $ nextByLength p (n - 1)

cairoPathDataTHeaderType :: Ptr CairoPathDataT -> IO #{type cairo_path_data_type_t}
cairoPathDataTHeaderType = #{peek cairo_path_data_t, header.type}

cairoPathDataTHeaderLength :: Ptr CairoPathDataT -> IO CInt
cairoPathDataTHeaderLength = #{peek cairo_path_data_t, header.length}

cairoPathDataTPointX, cairoPathDataTPointY :: Ptr CairoPathDataT -> IO CDouble
cairoPathDataTPointX = #{peek cairo_path_data_t, point.x}
cairoPathDataTPointY = #{peek cairo_path_data_t, point.y}

-- cairoPathTGetPathList :: Ptr CairoPathT ->
