{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Juicy where

import Data.Bits
import Data.Word
import Codec.Picture

writeArgbPng :: FilePath -> [[Word32]] -> IO ()
writeArgbPng fp = writePng fp . argbToImage

argbToImage :: [[Word32]] -> Image PixelRGBA8
argbToImage wss = generateImage
	(\x y -> word32ToPixel $ (wss !! y) !! x)
	(length $ head wss) (length wss)

word32ToPixel :: Word32 -> PixelRGBA8
word32ToPixel w = PixelRGBA8 r g b a
	where
	[a, r, g, b] = fromIntegral <$> [
		w `shiftR` 24, w `shiftR` 16, w `shiftR` 8, w ]
