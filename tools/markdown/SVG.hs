module SVG (
	textToSVG
) where

import Text
import Text.XML.YJSVG

textToSVG :: Double -> [Text] -> [String]
textToSVG r = map (showSVG (width r) (height r)) . textToSVGData r (topMargin r)

width, height :: Double -> Double
width = (3535 *)
height = (5000 *)

topMargin :: Double -> Double
topMargin = (200 *)

largest :: Double -> Double
largest = (500 *)

header :: Int -> Double -> Double
header i = ((1 / fromIntegral (i + 2)) * 1000 *)

textToSVGData :: Double -> Double -> [Text] -> [[SVG]]
textToSVGData r h [] = [[]]
textToSVGData r h (Header n s : ts)
	| h > height r - largest r = [l] : all
	| otherwise = (l : one) : rest
	where
	l = Text (TopLeft (100 * r) (h + header n r)) (header n r) (ColorName "black") "Kochi-Gothic" s
	one : rest = textToSVGData r (h + header n r) ts
	all = textToSVGData r (topMargin r) ts
