{-# LANGUAGE LambdaCase, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Data.Word
import Codec.Picture
import System.Environment

import Grayscale

main :: IO ()
main = do
	[src, dst] <- getArgs
	readImage src >>= \case
		Right (ImageRGBA8 i) -> do
			print (imageWidth i, imageHeight i)
			let	i' = pixelMap (\(PixelRGBA8 r g b a) ->
					fromDouble . uncurry3 grayscale $ rgb r g b a) i
			print (imageWidth i', imageHeight i')
			writePng dst i'
		_ -> error "no RGBA8 image"
	putStrLn "GRAYSCALE"

mulAlpha :: Word8 -> Word8 -> Word8
(fromIntegral -> n) `mulAlpha` (fromIntegral -> a) = fromIntegral (n * a `div` 0xff :: Word16)

rgb :: Word8 -> Word8 -> Word8 -> Word8 -> (Double, Double, Double)
rgb r g b a = (
	toDouble $ r `mulAlpha` a,
	toDouble $ g `mulAlpha` a,
	toDouble $ b `mulAlpha` a )

toDouble :: Word8 -> Double
toDouble = (/ 0xff) . fromIntegral

fromDouble :: Double -> Word8
fromDouble = round . (* 0xff)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z
