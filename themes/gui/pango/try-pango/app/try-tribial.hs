{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Graphics.Pango.PangoFixed

eight :: PangoFixed
eight = 8

minBoundPangoFixed :: PangoFixed
minBoundPangoFixed = toPangoFixed minBound

maxBoundPangoFixed :: PangoFixed
maxBoundPangoFixed = toPangoFixed maxBound

main :: IO ()
main = do
	print eight
	print $ fromPangoFixed eight
	print $ toPangoFixed 8192
	print minBoundPangoFixed
	print maxBoundPangoFixed
	print $ minBoundPangoFixed - 1
	print . fromPangoFixed $ minBoundPangoFixed - 1
