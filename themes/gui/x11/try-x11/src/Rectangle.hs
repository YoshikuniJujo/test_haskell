{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Rectangle where

import Graphics.X11 (Position, Dimension)

type Rectangle = ((Position, Position, Dimension, Dimension), Int)

inside :: (Position, Position) -> Rectangle -> Bool
inside (x, y) ((ulx, uly, w, h), _)
	| ulx <= x && x <= ulx + fromIntegral w &&
		uly <= y && y <= uly + fromIntegral h = True
	| otherwise = False

remove1 :: (Position, Position) -> [Rectangle] -> [Rectangle]
remove1 _ [] = []
remove1 p (r : rs)
	| p `inside` r = rs
	| otherwise = r : remove1 p rs
