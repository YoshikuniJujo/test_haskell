{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.List qualified as L
import Data.Vector.Storable qualified as V
import Data.Complex
import Data.Word
import Text.Read
import Codec.Picture
import System.Environment

import Control.Parallel.Strategies

sample :: Image PixelRGBA8
sample = Image 1000 750 $ V.replicate (4 * 1000 * 750) 0x7f

sample' :: Image Pixel8
sample' = Image 1000 750 $ V.replicate (1000 * 750) 0x7f

main :: IO ()
main = do
	[sz_, ul_, lr_] <- getArgs
	let	msz = parsePair sz_ 'x'
		mul = parsePair ul_ ','
		mlr = parsePair lr_ ','
	case (msz, mul, mlr) of
		(Just sz@(fromIntegral -> w, fromIntegral -> h), Just (lft, upr), Just (rgt, lwr)) ->
			writePng @Pixel8 "autogen/mandelbrot_hs.png"
				. Image w h $ render sz (lft :+ upr) (rgt :+ lwr)
		_ -> error "bad command line arguments"

escapeTime :: Complex Float -> Word32 -> Word32 -> Complex Float -> Maybe Word32
escapeTime z i lm c
	| i > lm  = Nothing
	| magnitude z > 2 = Just i
	| otherwise = escapeTime (z * z + c) (i + 1) lm c

pixelToPoint :: (Word32, Word32) -> (Word32, Word32) ->
	Complex Float -> Complex Float -> Complex Float
pixelToPoint (w, h) (x, y) (lft :+ upr) (rgt :+ lwr) =
	(lft + fromIntegral x * wdt / fromIntegral w) :+
	(upr - fromIntegral y * hgt / fromIntegral h)
	where wdt = rgt - lft; hgt = upr - lwr

render :: (Word32, Word32) -> Complex Float -> Complex Float -> V.Vector Word8
render wh@(w, h) ul lr =
--	V.generate (fromIntegral w * fromIntegral h) \(fromIntegral -> i) -> let
	V.fromList $ (\f -> parMap rseq f [0 .. w * h - 1]) \(fromIntegral -> i) -> let
		pxl = (i `mod` w, i `div` w)
		pnt = pixelToPoint wh pxl ul lr in
		case escapeTime 0 0 255 pnt of
			Nothing -> 0
			Just cnt -> 255 - fromIntegral cnt

parsePair :: Read a => String -> Char -> Maybe (a, a)
parsePair s c = case L.findIndex (== c) s of
	Nothing -> Nothing
	Just i -> (,) <$> (readMaybe $ take i s) <*> (readMaybe . tail $ drop i s)
