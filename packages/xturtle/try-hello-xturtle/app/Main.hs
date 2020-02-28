module Main where

import Graphics.X11.Turtle

main :: IO ()
main = do
	f <- openField
	t <- newTurtle f
	shikaku t 180 120
	penup t
	goto t 100 (- 100)
	pendown t
	shikaku t 100 80
	{-
	forward t 120
	left t 90
	forward t 120
	left t 90
	forward t 120
	left t 90
	forward t 120
	left t 90
	-}
	waitField f

shikaku :: Turtle -> Double -> Double -> IO ()
shikaku t w h = do
	forward t w
	right t 90
	forward t h
	right t 90
	forward t w
	right t 90
	forward t h
	right t 90
