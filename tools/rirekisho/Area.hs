{-# LANGUAGE PackageImports #-}

module Area (
	Area(..), areaOK,
	AreaM, runAreaM,
	mkArea,
	hSepArea,
	vSepArea,
	hSepAreaLog,
	vSepAreaLog,
	addStr,
	addMLStr,
	Draw(..),
	Position, PosX(..), PosY(..)
) where

import "monads-tf" Control.Monad.Writer
import Prelude hiding (Left, Right)

type Pos = Double
type Len = Double
type Size = Double
type Dot = Bool
type Bold = Bool

data Draw
	= Rectangle Pos Pos Len Len
	| HLine Dot Pos Pos Len
	| VLine Dot Pos Pos Len
	| Str Bold Pos Pos Size String
	deriving Show

data Area
	= Area Pos Pos Len Len
	deriving Show

areaOK :: Area -> Bool
areaOK (Area _ _ w h) = w > 0 && h > 0

type Position = (PosX, PosY)

data PosX = Left | Center | Right deriving Show
data PosY = Upper | Top | Middle | Bottom | Lower deriving Show

type AreaM = Writer [Draw]
runAreaM :: AreaM a -> [Draw]
runAreaM = execWriter

mkArea :: Pos -> Pos -> Len -> Len -> AreaM Area
mkArea x y w h = do
	tell $ [Rectangle x y w h]
	return $ Area x y w h

hSepArea :: Area -> Bool -> Len -> AreaM (Area, Area)
hSepArea (Area x y w h) dot r = do
	let	h' = r
		h'' = h - h'
		y' = y + h'
	tell [HLine dot x y' w]
	return (Area x y w h', Area x y' w h'')

hSepAreaLog :: Area -> Len -> AreaM (Area, Area)
hSepAreaLog (Area x y w h) r = do
	let	h' = r
		h'' = h - h'
		y' = y + h'
	return (Area x y w h', Area x y' w h'')

vSepArea :: Area -> Bool -> Len -> AreaM (Area, Area)
vSepArea (Area x y w h) dot r = do
	let	w' = r
		w'' = w - w'
		x' = x + w'
	tell [VLine dot x' y h]
	return (Area x y w' h, Area x' y w'' h)

vSepAreaLog :: Area -> Len -> AreaM (Area, Area)
vSepAreaLog (Area x y w h) r = do
	let	w' = r
		w'' = w - w'
		x' = x + w'
	return (Area x y w' h, Area x' y w'' h)

addStr :: Area -> Position -> Bold -> Size -> String -> AreaM ()
addStr (Area x y w h) (px, py) b s str =
	tell [Str b (getX px x w s $ myLength str) (getY py y h s) s str]

addMLStr :: Area -> Position -> Size -> String -> AreaM ()
addMLStr (Area x y w h) (px, py) s str =
	tell $ zipWith (\str' y' ->
		Str False (getX px x w s $ myLength str') y' s str')
		(reverse sepStr) $
		iterate (subtract s) $ getY py y h $
			s * fromIntegral (length sepStr)
	where
	charPLine = floor $ w / s
	sepStr = nGroups (charPLine - 2) str

nGroups :: Int -> [a] -> [[a]]
nGroups _ [] = []
nGroups n xs = take n xs : nGroups n (drop n xs)

myLength :: String -> Int
myLength "" = 0
myLength (c : cs)
	| c `elem` (['0' .. '9'] ++ ['a' .. 'z'] ++ ['A' .. 'Z']) =
		1 + myLength cs
	| otherwise = 2 + myLength cs

getX :: PosX -> Pos -> Len -> Size -> Int -> Pos
getX Left x _ s _ = x + s / 2
getX Center x w s i = x + (w - s / 2 * fromIntegral i) / 2
getX Right x w s i = x + w - s / 2 * fromIntegral i

getY :: PosY -> Pos -> Len -> Size -> Pos
getY Upper y _ s = y - s
getY Top y _ s = y + s
getY Middle y h s = y + (h + s) / 2
getY Bottom y h s = y + h - s / 4
getY Lower y h s = y + h + s
