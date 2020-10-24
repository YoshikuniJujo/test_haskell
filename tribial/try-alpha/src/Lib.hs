{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Codec.Picture

checkFormat :: FilePath -> IO String
checkFormat fp = readPng fp >>= \case
	Right (ImageRGB8 _) -> pure "ImageRGB8"
	Right (ImageRGB16 _) -> pure "ImageRGB16"
	Right (ImageRGBA8 _) -> pure "ImageARGA8"
	Right (ImageRGBA16 _) -> pure "ImageARGA8"
	Left em -> pure $ "error: " ++ em
	_ -> pure "others"

{-
addAlpha :: FilePath -> FilePath -> IO ()
addAlpha src dst = readPng src >>= \case
	Left em -> error $ "error: " ++ em
	Right di -> writePng dst $ convertRGBA8 di
	-}

readPngImageRGB8 :: FilePath -> IO (Image PixelRGB8)
readPngImageRGB8 fp = readPng fp >>= \case
	Right (ImageRGB8 i) -> pure i
	_ -> error "no format"

addAlpha :: PixelRGB8 -> PixelRGBA8
addAlpha (PixelRGB8 r g b)
--	| b > 0x8f = PixelRGBA8 r g b 0xff
	| r > 0x6f && g > 0x6f && b > 0x6f = PixelRGBA8 r g b 0x00
	| otherwise = PixelRGBA8 r g b 0xff

convertPngFile :: FilePath -> FilePath -> IO ()
convertPngFile src dst = writePng dst . pixelMap addAlpha =<< readPngImageRGB8 src
