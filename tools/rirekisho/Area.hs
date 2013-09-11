{-# LANGUAGE PackageImports #-}

module Area (
	AreaM, runAreaM,
	mkArea,
	hSepArea,
	addStr,
	Draw(..),
	Position, PosX(..), PosY(..)
) where

import "monads-tf" Control.Monad.Writer

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

type Position = (PosX, PosY)

data PosX = Left | Center deriving Show
data PosY = Upper | Top | Middle | Bottom | Lower deriving Show

type AreaM = Writer [Draw]
runAreaM :: AreaM a -> [Draw]
runAreaM = execWriter

mkArea :: Pos -> Pos -> Len -> Len -> AreaM Area
mkArea x y w h = do
	tell $ [Rectangle x y w h]
	return $ Area x y w h

hSepArea :: Double -> Area -> AreaM (Area, Area)
hSepArea r (Area x y w h) = do
	let	h' = h * r / 100
		h'' = h - h'
		y' = y + h'
	tell $ [HLine False x y' w]
	return (Area x y w h') (Area x y' w h'')

addStr :: Area -> Position -> Bold -> Size -> String -> AreaM ()
addStr (Area x y w h) (px, py) b s str =
	tell [Str b (getX px x w s $ length str) (getY py y h s) s str]

getX :: PosX -> Pos -> Len -> Size -> Int -> Pos
getX Center x w s i = x + (w - s * fromIntegral i) / 2

getY :: PosY -> Pos -> Len -> Size -> Pos
getY Middle y h s = y + (h + s) / 2
