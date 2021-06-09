{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Graphics.Pango.Basic.GlyphStorage

eight :: PangoFixed
eight = 8

minBoundPangoFixed :: PangoFixed
minBoundPangoFixed = fromCInt minBound

maxBoundPangoFixed :: PangoFixed
maxBoundPangoFixed = fromCInt maxBound

main :: IO ()
main = do
	print eight
	print $ toCInt eight
	print $ fromCInt 8192
	print minBoundPangoFixed
	print maxBoundPangoFixed
	print $ minBoundPangoFixed - 1
	print . toCInt $ minBoundPangoFixed - 1

	print $ PangoRectangleFixed 3 8 20 25
