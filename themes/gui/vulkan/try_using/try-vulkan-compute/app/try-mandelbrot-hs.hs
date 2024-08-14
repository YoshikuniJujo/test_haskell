{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Arrow
import Control.Parallel.Strategies
import Data.Vector.Storable qualified as V
import Data.Word
import Data.Complex
import Mandelbrot.Draw

main :: IO ()
main = draw "autogen/mandelbrot_hs.png" \sz lt rb ->
	pure . V.fromList $ render sz lt rb

render :: Size -> Complex Float -> Complex Float -> [Word8]
render wh@(w, h) lt rb = (\f -> parMap rseq f [0 .. w * h - 1])
	$ maybe 0 ((255 -) . fromIntegral) . escapeTime 0 0 255
		. pxToPnt wh lt rb . ((`mod` w) &&& (`div` w))

escapeTime :: Complex Float -> Word32 -> Word32 -> Complex Float -> Maybe Word32
escapeTime z i lm c
	| i > lm  = Nothing
	| magnitude z > 2 = Just i
	| otherwise = escapeTime (z * z + c) (i + 1) lm c

pxToPnt :: Size -> Complex Float -> Complex Float -> Pos -> Complex Float
pxToPnt (fromIntegral -> pxw, fromIntegral -> pxh)
	(l :+ t) (r :+ b) (fromIntegral -> x, fromIntegral -> y) =
	(l + x * (r - l) / pxw) :+ (t - y * (t - b) / pxh)
