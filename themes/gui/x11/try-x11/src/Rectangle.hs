{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Rectangle (
	Rectangle, Color(..),
	calcRectangle, pushRectangle, dragRectangle, floatHead,
	rotColor, sink1, remove1
	) where

import Data.Bool
import Numeric.Natural

data Rectangle = Rectangle {
	left :: Integer,
	upper :: Integer,
	right :: Integer,
	bottom :: Integer,
	color :: Color,
	floating :: Bool
	} deriving Show

data Color = Red | Green | Blue | Yellow | Cyan | Magenta deriving (Show, Enum)

rotate :: Color -> Color
rotate Magenta = Red
rotate c = succ c

calcRectangle :: Double -> Double -> Rectangle ->
	((Integer, Integer, Natural, Natural), Color)
calcRectangle amp agl Rectangle {
	left = l, upper = u, right = r, bottom = b, color = c, floating = f } =
	(	(min l r + bool 0 (round $ amp * sin agl) f, min u b,
			fromIntegral . abs $ r - l, fromIntegral . abs $ b - u),
		c)

pushRectangle :: (Integer, Integer) -> [Rectangle] -> [Rectangle]
pushRectangle (l, u) rs = (: rs) $ Rectangle {
	left = l, upper = u, right = l, bottom = u,
	color = Red, floating = False }

dragRectangle :: (Integer, Integer) -> [Rectangle] -> [Rectangle]
dragRectangle _ [] = error "no rectangles"
dragRectangle (x, y) (r : rs) = r { right = x, bottom = y } : rs

floatHead :: [Rectangle] -> [Rectangle]
floatHead [] = error "no rectangles"
floatHead (r : rs) = r { floating = True } : rs

rotColor :: (Integer, Integer) -> [Rectangle] -> [Rectangle]
rotColor _ [] = []
rotColor p (r@Rectangle { floating = True } : rs) | p `inside` r =
	r { color = rotate $ color r } : rs
rotColor p (r : rs) = r : rotColor p rs

sink1 :: (Integer, Integer) -> [Rectangle] -> [Rectangle]
sink1 _ [] = []
sink1 p (r@Rectangle { floating = True } : rs) | p `inside` r =
	r { floating = False } : rs
sink1 p (r : rs) = r : sink1 p rs

remove1 :: (Integer, Integer) -> [Rectangle] -> [Rectangle]
remove1 _ [] = []
remove1 p (r : rs) | p `inside` r = rs | otherwise = r : remove1 p rs

inside :: (Integer, Integer) -> Rectangle -> Bool
inside (x, y) Rectangle { left = l, upper = u, right = r, bottom = b } =
	(l <= x && x <= r || r <= x && x <= l) &&
	(u <= y && y <= b || b <= y && y <= u)
