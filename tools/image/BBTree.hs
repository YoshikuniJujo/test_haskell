module BBTree (value, box, emptyTree, arrow) where

import Control.Monad
import Graphics.X11.Turtle

value :: Turtle -> Double -> IO ()
value t s = do
	setheading t 0
	pendown t
	circle t (s * 4)
	penup t

emptyTree :: Turtle -> Double -> IO ()
emptyTree t s = do
	(x0, y0) <- position t
	box t s
	goto t (x0 + 3 * s) (y0 + 8 * s)
	write t "KochiGothic" (8 * s) "N"

box :: Turtle -> Double -> IO ()
box t s = do
	(x0, y0) <- position t
	setheading t (- 90)
	pendown t
	replicateM_ 4 $ forward t (10 * s) >> left t 90
	penup t

arrow :: Turtle -> Double -> IO ()
arrow t s = do
	beginfill t
	pendown t
	forward t (5 * s)
	left t 90
	forward t (2 * s)
	right t 135
	forward t (3 * 2 ** (1 / 2) * s)
	right t 90
	forward t (3 * 2 ** (1 / 2) * s)
	right t 135
	forward t (2 * s)
	left t 90
	forward t (5 * s)
	right t 90
	forward t (2 * s)
	penup t
	endfill t
