module SVG (
	textToSVG
) where

import Control.Applicative
import Data.Char
import Text.XML.YJSVG

import Text

textToSVG :: Double -> [Text] -> [String]
textToSVG r = map (showSVG (width r) (height r)) . textToSVGData r (topMargin r)

width, height :: Double -> Double
width = (3535 *)
height = (5000 *)

topMargin, leftMargin, paraLeftMargin :: Double -> Double
topMargin = (400 *)
leftMargin = (400 *)
paraLeftMargin = (+) <$> leftMargin <*> (200 *)

bottomMargin :: Double -> Double
bottomMargin = (400 *)

bottomBorder :: Double -> Double
bottomBorder = (-) <$> height <*> bottomMargin

header :: Int -> Double -> Double
header i = ((1 / fromIntegral (i + 2)) * 1000 *)

normal :: Double -> Double
normal = (100 *)

normalSep :: Double -> Double
normalSep = (* (4 / 3)) <$> normal

lineChars :: Int
lineChars = 75

textToSVGData :: Double -> Double -> [Text] -> [[SVG]]
textToSVGData r h [] = [[]]
textToSVGData r h (Header n s : ts)
	| h > bottomBorder r - header n r = [l] : all
	| otherwise = (l : one) : rest
	where
	l = Text (TopLeft (leftMargin r) (h + header n r)) (header n r) (ColorName "black") "Kochi-Gothic" s
	one : rest = textToSVGData r (h + header n r * 3 / 2) ts
	all = textToSVGData r (topMargin r) ts
textToSVGData r h (Paras [] : ts) = textToSVGData r h ts
textToSVGData r h (Paras (p : ps) : ts)
	| h' > bottomBorder r = [] : (svgs' ++ one') : rest'
	| otherwise = (svgs ++ one) : rest
	where
	(h', svgs) = paraToSVGData r h p
	(h'', svgs') = paraToSVGData r (topMargin r) p
	one : rest = textToSVGData r h' (Paras ps : ts)
	one' : rest' = textToSVGData r h'' (Paras ps : ts)
-- textToSVGData r h (List l)
--	| h' > bottomBor

splitAtString :: Int -> String -> (String, String)
splitAtString len = sepStr 0
	where
	sepStr _ "" = ("", "")
	sepStr n (c : c'@('。') : cs)
		|  n > len = ([c, c'], cs)
		| otherwise = let (s, t) = sepStr (n + 6) cs in (c : c' : s, t)
	sepStr n (c : cs)
		| n > len = ([c], cs)
		| isAscii c = let (s, t) = sepStr (n + 2) cs in (c : s, t)
		| otherwise = let (s, t) = sepStr (n + 3) cs in (c : s, t)

separateString :: Int -> String -> [String]
separateString len = sepStr 0
	where
	sepStr _ "" = [[]]
	sepStr n (c : c'@('。') : cs)
		|  n > len = [c, c'] : sepStr 0 cs
		| otherwise = let s : ss = sepStr (n + 6) cs in (c : c' : s) : ss
	sepStr n (c : cs)
		| n > len = [c] : sepStr 0 cs
		| isAscii c = let s : ss = sepStr (n + 2) cs in (c : s) : ss
		| otherwise = let s : ss = sepStr (n + 3) cs in (c : s) : ss

paraToSVGData :: Double -> Double -> String -> (Double, [SVG])
paraToSVGData r h str = (h', l : svgs)
	where
	(s, t) = splitAtString (lineChars - 3) str
	l = Text (TopLeft (paraLeftMargin r + normal r) (h + normal r)) (normal r) (ColorName "black") "Kochi-Gothic" s
	ls = separateString lineChars t
	(h', svgs) = strsToSVGData r (h + normalSep r) ls

strsToSVGData :: Double -> Double -> [String] -> (Double, [SVG])
strsToSVGData r h [] = (h, [])
strsToSVGData r h ([] : ss) = strsToSVGData r h ss
strsToSVGData r h (s : ss) = (h', l : svgs)
	where
	l = Text (TopLeft (paraLeftMargin r) (h + normal r)) (normal r) (ColorName "black") "Kochi-Gothic" s
	(h', svgs) = strsToSVGData r (h + normalSep r) ss
