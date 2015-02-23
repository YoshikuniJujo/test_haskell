module Machine2 (
	machine, armA, armTup,
	) where

import Control.Monad
import Text.XML.YJSVG hiding (topleft)
import Graphics.X11.Turtle

machine :: ColorClass c =>
	Turtle -> c -> (Turtle -> c -> Double -> IO ()) -> Double -> IO ()
machine t c a s = do
	(x, y) <- position t
	body t c s
	goto t (x + 8 * s) y
	a t c s

armA :: ColorClass c => Turtle -> c -> Double -> IO ()
armA t c s = do
	(x, y) <- position t
	penup t
	pencolor t c
	setheading t 0

	setheading t 0
	beginfill t
	forward t $ 6 * s
	right t 90
	forward t $ 2 * s

	right t 90
	forward t $ 2 * s
	left t 90
	forward t $ 4 * s
	left t 90
	forward t $ 2 * s
	right t 90

	forward t $ 2 * s
	right t 90
	forward t $ 6 * s
	endfill t

body :: ColorClass c => Turtle -> c -> Double -> IO ()
body t c s = do
	(x, y) <- position t
	goto t x (y + 10 * s)
	penup t
	pencolor t c
	setheading t 0
	beginfill t
	replicateM_ 2 $
		forward t (16 * s) >> right t 90 >> forward t (8 * s) >> right t 90
	endfill t

	goto t (x + 2 * s) (y + 6 * s)
	beginfill t
	replicateM_ 2 $
		forward t (4 * s) >> right t 90 >> forward t (4 * s) >> right t 90
	endfill t

	goto t (x + 2 * s) (y + 2 * s)
	beginfill t
	replicateM_ 2 $
		forward t (6 * s) >> right t 90 >> forward t (4 * s) >> right t 90
	endfill t

convertor :: ColorClass c => Turtle -> c -> Double -> IO ()
convertor t c s = do
	penup t
	pencolor t c
	setheading t 0
	forward t $ 2 * s
	beginfill t
	forward t $ 6 * s
	setheading t (- 150)
	replicateM_ 13 $ forward t (pi / 5.5 * s) >> left t 10
	setheading t 0
	backward t $ 6 * s
	setheading t 90
	forward t $ 1.4 * s
	left t 90
	forward t $ 2 * s
	right t 90
	forward t $ 3 * s
	right t 90
	forward t $ 2 * s
	left t 90
	forward t s
	{-
	right t 90
	forward t $ 8 * s
	-}
	endfill t

armTup :: ColorClass c => Turtle -> c -> Double -> IO ()
armTup t c s = do
	(x, y) <- position t
	goto t x (y - 8 * s)
	setheading t 0
	beginfill t
	forward t $ 6 * s

	right t 90
	forward t $ 2 * s
	setheading t (- 150)
	replicateM_ 13 $ forward t (pi / 5.2 * s) >> left t 10
	
	setheading t (- 90)
	forward t $ 1 * s

	setheading t (- 120)
	forward t $ 4 * s
	setheading t 0
	forward t $ 2 * s

	setheading t (- 90)
	forward t $ 2 * s

	forward t $ 2 * s

	right t 90
	forward t $ 6 * s
	endfill t
