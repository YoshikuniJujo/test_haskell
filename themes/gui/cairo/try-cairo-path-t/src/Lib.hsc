{-# LANGUAGE BlockArguments, LambdaCase #-}
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

import System.IO.Unsafe

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

data Path
	= MoveTo CDouble CDouble
	| LineTo CDouble CDouble
	| CurveTo CDouble CDouble CDouble CDouble CDouble CDouble
	| ClosePath
	deriving Show

cairoPathTPathList :: Ptr CairoPathT -> IO [Path]
cairoPathTPathList p = do
	d <- cairoPathTData p
	n <- cairoPathTNumData p
	cairoPathDataTPathList d n

cairoPathDataTPathList :: Ptr CairoPathDataT -> CInt -> IO [Path]
cairoPathDataTPathList _ n | n < 1 = pure []
cairoPathDataTPathList p n = unsafeInterleaveIO do
	pth <- cairoPathDataTHeaderType p >>= \case
		#{const CAIRO_PATH_MOVE_TO} -> MoveTo
			<$> cairoPathDataTPointX p1 <*> cairoPathDataTPointY p1
		#{const CAIRO_PATH_LINE_TO} -> LineTo
			<$> cairoPathDataTPointX p1 <*> cairoPathDataTPointY p1
		#{const CAIRO_PATH_CURVE_TO} -> CurveTo
			<$> cairoPathDataTPointX p1 <*> cairoPathDataTPointY p1
			<*> cairoPathDataTPointX p2 <*> cairoPathDataTPointY p2
			<*> cairoPathDataTPointX p3 <*> cairoPathDataTPointY p3
		#{const CAIRO_PATH_CLOSE_PATH} -> pure ClosePath
		_ -> error "no such path"
	ln <- cairoPathDataTHeaderLength p
	(pth :) <$> cairoPathDataTPathList (nextByLength p ln) (n - ln)
	where
	p1 = nextByLength p 1
	p2 = nextByLength p 2
	p3 = nextByLength p 3
