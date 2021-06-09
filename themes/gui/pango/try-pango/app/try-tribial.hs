{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Exception
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
	print (toCInt $ minBoundPangoFixed - 1) `catch` print @ArithException

	let	r = PangoRectangleFixed 3 8 20 25
	print r
	print $ r { pangoRectangleFixedWidth = 28 }
