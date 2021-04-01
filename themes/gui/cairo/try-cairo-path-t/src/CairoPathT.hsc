{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module CairoPathT (Path(..), CairoPathT, pattern CairoPathT, mkCairoPathT) where

import Foreign.Ptr
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Concurrent
import Foreign.Marshal
import Foreign.Storable
import Foreign.C.Types
import Data.Word

import Graphics.Cairo.Exception

import System.IO.Unsafe

#include <cairo.h>

newtype CairoPathT = CairoPathT_ (ForeignPtr CairoPathT) deriving Show

pattern CairoPathT :: [Path] -> CairoPathT
pattern CairoPathT ps <- (unsafePerformIO . cairoPathTPathList -> ps) where
	CairoPathT = unsafePerformIO . pathListToCairoPathT

mkCairoPathT :: Ptr CairoPathT -> IO CairoPathT
mkCairoPathT p =
	CairoPathT_ <$> newForeignPtr p (c_cairo_path_destroy p)
		<* (cairoStatusToThrowError =<< cairoPathTStatus p)

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

cairoPathTPathList :: CairoPathT -> IO [Path]
cairoPathTPathList (CairoPathT_ fp) = withForeignPtr fp \p -> do
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

pathToNumData :: Path -> Int
pathToNumData = \case
	MoveTo _ _ -> 2; LineTo _ _ -> 2; CurveTo _ _ _ _ _ _ -> 4; ClosePath -> 1

pathToCairoPathData :: Ptr CairoPathDataT -> Path -> IO ()
pathToCairoPathData p = \case
	MoveTo x y -> do
		#{poke cairo_path_data_t, header.type} p (#{const CAIRO_PATH_MOVE_TO} :: #{type cairo_path_data_type_t})
		#{poke cairo_path_data_t, header.length} p (2 :: CInt)
		#{poke cairo_path_data_t, point.x} p1 x
		#{poke cairo_path_data_t, point.y} p1 y
	LineTo x y -> do
		#{poke cairo_path_data_t, header.type} p (#{const CAIRO_PATH_LINE_TO} :: #{type cairo_path_data_type_t})
		#{poke cairo_path_data_t, header.length} p (2 :: CInt)
		#{poke cairo_path_data_t, point.x} p1 x
		#{poke cairo_path_data_t, point.y} p1 y
	CurveTo x1 y1 x2 y2 x3 y3 -> do
		#{poke cairo_path_data_t, header.type} p (#{const CAIRO_PATH_CURVE_TO} :: #{type cairo_path_data_type_t})
		#{poke cairo_path_data_t, header.length} p (4 :: CInt)
		#{poke cairo_path_data_t, point.x} p1 x1
		#{poke cairo_path_data_t, point.y} p1 y1
		#{poke cairo_path_data_t, point.x} p2 x2
		#{poke cairo_path_data_t, point.y} p2 y2
		#{poke cairo_path_data_t, point.x} p3 x3
		#{poke cairo_path_data_t, point.y} p3 y3
	ClosePath -> do
		#{poke cairo_path_data_t, header.type} p (#{const CAIRO_PATH_CLOSE_PATH} :: #{type cairo_path_data_type_t})
		#{poke cairo_path_data_t, header.length} p (1 :: CInt)
	where
	p1 = nextByLength p 1
	p2 = nextByLength p 2
	p3 = nextByLength p 3

calcAlignedSize :: Int -> Int -> Int
calcAlignedSize sz al = (sz `div` al + signum (sz `mod` al)) * al

cairoPathDataTSize :: Int
cairoPathDataTSize = calcAlignedSize #{size cairo_path_data_t} #{alignment cairo_path_data_t}

pathListToCairoPathT :: [Path] -> IO CairoPathT
pathListToCairoPathT pths = CairoPathT_ <$> do
	pd <- mallocBytes $ sum (pathToNumData <$> pths) * cairoPathDataTSize
	pathListToCairoPathDataT pd pths
	p <- mallocBytes #{size cairo_path_t}
	#{poke cairo_path_t, status} p (#{const CAIRO_STATUS_SUCCESS} :: #{type cairo_status_t})
	#{poke cairo_path_t, data} p pd
	#{poke cairo_path_t, num_data} p $ sum (pathToNumData <$> pths)
	newForeignPtr p $ free pd >> free p

pathListToCairoPathDataT :: Ptr CairoPathDataT -> [Path] -> IO ()
pathListToCairoPathDataT pd = \case
	[] -> pure ()
	pth : pths -> pathToCairoPathData pd pth >> pathListToCairoPathDataT (pd `plusPtr` (cairoPathDataTSize * pathToNumData pth)) pths

