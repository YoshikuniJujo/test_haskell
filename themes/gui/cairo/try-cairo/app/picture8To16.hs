{-# LANGUAGE BlockArguments, LambdaCase, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Data.Bits
import Codec.Picture
import System.Environment

main :: IO ()
main = do
	[src, dst] <- getArgs
	readImage src >>= \case
		Right (ImageRGB8 i) -> do
			print (imageWidth i, imageHeight i)
			let	i' = pixelMap (\(PixelRGB8
						(fromIntegral -> r) (fromIntegral -> g) (fromIntegral -> b)) ->
					PixelRGB16 (r `shiftL` 8 .|. r) (g `shiftL` 8 .|. g) (b `shiftL` 8 .|. b)) i
			print (imageWidth i', imageHeight i')
			writePng dst i'
		_ -> error "no RGB8 image"
	putStrLn "REMOVE ALPHA"
