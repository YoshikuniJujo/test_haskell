{-# LANGUAGE BlockArguments, LambdaCase, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Data.Word
import Codec.Picture
import System.Environment

main :: IO ()
main = do
	[src, dst] <- getArgs
	readImage src >>= \case
		Right (ImageRGBA8 i) -> do
			print (imageWidth i, imageHeight i)
			let	i' = pixelMap (\(PixelRGBA8 r g b a) ->
					PixelRGB8 (r `mulAlpha` a) (g `mulAlpha` a) (b `mulAlpha` a)) i
			print (imageWidth i', imageHeight i')
			writePng dst i'
		_ -> error "no RGBA8 image"
	putStrLn "REMOVE ALPHA"

mulAlpha :: Word8 -> Word8 -> Word8
(fromIntegral -> n) `mulAlpha` (fromIntegral -> a) = fromIntegral (n * a `div` 0xff :: Word16)
