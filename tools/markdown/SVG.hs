module SVG (
	textToSVG
) where

import Control.Applicative
import Data.Char
import Text.XML.YJSVG

import Text

headerFont, normalFont, codeFont :: String
headerFont = "Kochi Gothic"
normalFont = "Kochi Mincho"
-- normalFont = "Kochi Gothic"
codeFont = "Kochi Gothic"

textToSVG :: Bool -> Double -> [Text] -> [String]
textToSVG n r = map (showSVG (width r) (height r)) . textToSVGData r (topMargin r) .
	if n then addChapters [Nothing, Just 0, Just 0, Just 0, Nothing, Nothing] else id

width, height :: Double -> Double
width = (3535 *)
height = (5000 *)

topMargin, leftMargin, paraLeftMargin :: Double -> Double
topMargin = (400 *)
leftMargin = (400 *)
paraLeftMargin = (+) <$> leftMargin <*> (200 *)
codeLeftMargin = (+) <$> paraLeftMargin <*> (200 *)

bottomMargin :: Double -> Double
bottomMargin = (400 *)

bottomBorder :: Double -> Double
bottomBorder = (-) <$> height <*> bottomMargin

header :: Int -> Double -> Double
header i = ((1 / fromIntegral (i + 1)) * 560 *)
headerSep i = (* (4 / 3)) <$> header i

normal, normalSep :: Double -> Double
normal = (80 *)
normalSep = (* (4 / 3)) <$> normal

code, codeSep :: Double -> Double
code = (70 *)
codeSep = (* (4 / 3)) <$> code

lineChars :: Int
lineChars = 66

textToSVGData :: Double -> Double -> [Text] -> [[SVG]]
textToSVGData r h [] = [[]]
textToSVGData r h (Header n s : ts)
	| h > bottomBorder r - headerSep n r = [l] : all
	| otherwise = (l : one) : rest
	where
	l = Text (TopLeft (leftMargin r) (h + headerSep n r)) (header n r) (ColorName "black") headerFont s
	one : rest = textToSVGData r (h + headerSep n r * 5 / 4) ts
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
textToSVGData r h (List l : ts)
	| h' > bottomBorder r = [] : (svgs' ++ one') : rest'
	| otherwise = (svgs ++ one) : rest
	where
	(_, h', svgs) = listToSVGData 1 "*+-------" r h (paraLeftMargin r) l
	(_, h'', svgs') = listToSVGData 1 "*+-------" r (topMargin r) (paraLeftMargin r) l
	one : rest = textToSVGData r h' ts
	one' : rest' = textToSVGData r h'' ts
textToSVGData r h (Code s : ts)
	| h' > bottomBorder r = [] : (svgs' ++ one') : rest'
	| otherwise = (svgs ++ one) : rest
	where
	(h', svgs) = codeToSVGData r (h + codeSep r) (lines s)
	(h'', svgs') = codeToSVGData r (topMargin r) (lines s)
	one : rest = textToSVGData r (h' + codeSep r) ts
	one' : rest' = textToSVGData r (h'' + codeSep r) ts

splitAtString :: Int -> String -> (String, String)
splitAtString len = sepStr 0
	where
	sepStr _ "" = ("", "")
	sepStr n (c : c'@('ー') : cs)
		|  n > len = ([c, c'], cs)
		| otherwise = let (s, t) = sepStr (n + 4) cs in (c : c' : s, t)
	sepStr n (c : c'@('。') : cs)
		|  n > len = ([c, c'], cs)
		| otherwise = let (s, t) = sepStr (n + 4) cs in (c : c' : s, t)
	sepStr n (c : cs)
		| n > len = ([c], cs)
		| isAscii c = let (s, t) = sepStr (n + 1) cs in (c : s, t)
		| otherwise = let (s, t) = sepStr (n + 2) cs in (c : s, t)

separateString :: Int -> String -> [String]
separateString len = sepStr 0
	where
	sepStr _ "" = [[]]
	sepStr n (c : c'@('ー') : cs)
		|  n > len = [c, c'] : sepStr 0 cs
		| otherwise = let s : ss = sepStr (n + 4) cs in (c : c' : s) : ss
	sepStr n (c : c'@('。') : cs)
		|  n > len = [c, c'] : sepStr 0 cs
		| otherwise = let s : ss = sepStr (n + 4) cs in (c : c' : s) : ss
	sepStr n (c : cs)
		| n > len = [c] : sepStr 0 cs
		| isAscii c = let s : ss = sepStr (n + 1) cs in (c : s) : ss
		| otherwise = let s : ss = sepStr (n + 2) cs in (c : s) : ss

paraToSVGData :: Double -> Double -> String -> (Double, [SVG])
paraToSVGData r h str = (h', l : svgs)
	where
	(s, t) = splitAtString (lineChars - 3) str
	l = Text (TopLeft (paraLeftMargin r + normal r) (h + normal r)) (normal r) (ColorName "black") normalFont s
	ls = separateString lineChars t
	(h', svgs) = strsToSVGData r (h + normalSep r) ls

strsToSVGData :: Double -> Double -> [String] -> (Double, [SVG])
strsToSVGData r h [] = (h, [])
strsToSVGData r h ([] : ss) = strsToSVGData r h ss
strsToSVGData r h (s : ss) = (h', l : svgs)
	where
	l = Text (TopLeft (paraLeftMargin r) (h + normal r)) (normal r) (ColorName "black") normalFont s
	(h', svgs) = strsToSVGData r (h + normalSep r) ss

listToSVGData :: Int -> String -> Double -> Double -> Double -> List -> (Int, Double, [SVG])
listToSVGData n sym r y x [] = (n, y, [])
listToSVGData n sym r y x (lst : lsts) = (n'', y'', svgs ++ svgs')
	where
	(n', y', svgs) = list1ToSVGData n sym r y x lst
	(n'', y'', svgs') = listToSVGData n' sym r y' x lsts

list1ToSVGData :: Int -> String -> Double -> Double -> Double -> List1 -> (Int, Double, [SVG])
list1ToSVGData n sym r y x (BulItem s lst) = (n, h', l : svgs)
	where
	l = Text (TopLeft x (y + normal r)) (normal r) (ColorName "black") normalFont $ head sym : ' ' : s
	(_, h', svgs) = listToSVGData n (tail sym) r (y + normalSep r) (x + normal r * 2) lst
list1ToSVGData n sym r y x (OrdItem s lst) = (n + 1, h', l : svgs)
	where
	l = Text (TopLeft x (y + normal r)) (normal r) (ColorName "black") normalFont $ show n ++ '.' : ' ' : s
	(_, h', svgs) = listToSVGData n (tail sym) r (y + normalSep r) (x + normal r * 2) lst

codeToSVGData :: Double -> Double -> [String] -> (Double, [SVG])
codeToSVGData _ h [] = (h, [])
codeToSVGData r h (s : ss) = (h', l : svgs)
	where
	l = Text (TopLeft (codeLeftMargin r) (h + code r)) (code r) (ColorName "black") codeFont s
	(h', svgs) = codeToSVGData r (h + codeSep r) ss
