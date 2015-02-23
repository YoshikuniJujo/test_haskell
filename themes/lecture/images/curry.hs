import Control.Monad
import Text.XML.YJSVG hiding (topleft)
import Graphics.X11.Turtle

import Machine2

main :: IO ()
main = do
	f <- openField
	topleft f
	onkeypress f $ return . (/= 'q')
	t <- newTurtle f
	penup t

	goto t 60 5
	putCurry t "black" 4

	goto t 20 125
	tupMachine t "black" 4

	hideturtle t
	svg <- getSVG t
	writeFile "curry.svg" $ showSVG 205 205 svg
	waitField f

tupMachine :: ColorClass c => Turtle -> c -> Double -> IO ()
tupMachine t c s = do
	(x, y) <- position t
	machine t "black" armTup 4
	goto t (x + 12.5 * s) (y - 5.75 * s)
	tup t "black" 4

tup :: ColorClass c => Turtle -> c -> Double -> IO ()
tup t c s = do
	(x, y) <- position t
	goto t (x + 3 * s) y
	setheading t 0
	beginfill t
	replicateM_ 36 $ forward t (pi / 6 * s) >> right t 10
	endfill t
	goto t (x + 3 * s) (y + 5.5 * s)
	setheading t (- 120)
	beginfill t
	replicateM_ 3 $ forward t (5.2 * s) >> left t 120
	endfill t

putCurry :: ColorClass c => Turtle -> c -> Double -> IO ()
putCurry t c s = do
	(x, y) <- position t
	pencolor t c
	penup t
	setheading t 0
	goto t x y
	curryBody t c s
	goto t x y
	arm1 t c s
	goto t x y
	arm2 t c s
	goto t x y
	arm3 t c s

curryBody, arm1, arm2, arm3 :: ColorClass c => Turtle -> c -> Double -> IO ()
curryBody t c s = do
	beginfill t
	replicateM_ 2 $
		forward t (35 * s) >> right t 90 >> forward t (10 * s) >> right t 90
	endfill t
arm1 t c s = do
	(x, y) <- position t
	goto t (x - 11.5 * s) (y + 28.5 * s)
	beginfill t
	forward t $ 7 * s
	right t 90
	forward t $ 3 * s
	right t 90
	forward t $ 4 * s
	left t 90
	forward t $ 4 * s
	right t 90
	forward t $ 3 * s
	right t 90
	forward t $ 33 * s
	right t 90
	forward t $ 11.5 * s
	right t 90
	forward t $ 3 * s
	right t 90
	forward t $ 8.5 * s
	left t 90
	forward t $ 23 * s
	endfill t
arm2 t c s = do
	(x, y) <- position t
	setheading t 20
	goto t (x + 5.9 * s) (y + 22.1 * s)
	beginfill t
	forward t $ 6 * s
	right t 90
	forward t $ 9 * s
	right t 90
	forward t $ 6 * s
	right t 90
	forward t $ 1 * s
	right t 60
	replicateM_ 13 $ forward t (pi / 5.2 * s) >> left t 10
	right t 70
	forward t $ 1 * s
	endfill t
	goto t (x + 10 * s) (y + 23.5 * s)
	setheading t 0
	beginfill t
	forward t $ 10 * s
	left t 90
	forward t $ 13.5 * s
	right t 90
	forward t $ 3 * s
	right t 90
	forward t $ 16.5 * s
	right t 90
	forward t $ 13 * s
	endfill t
arm3 t c s = do
	(x, y) <- position t
	goto t (x + 7.5 * s) (y + 31.0 * s)
	setheading t (- 10)
	beginfill t
	forward t $ 6 * s
	right t 90
	forward t $ 7 * s
	right t 90
	forward t $ 6 * s
	right t 90
	forward t $ 3.2 * s
	setheading t 0
	forward t $ 2.2 * s
	left t 120
	forward t $ 3 * s
	endfill t
	goto t (x + 13 * s) (y + 34 * s)
	setheading t 0
	beginfill t
	forward t $ 15 * s
	left t 90
	forward t $ 24 * s
	right t 90
	forward t $ 3 * s
	right t 90
	forward t $ 27 * s
	right t 90
	forward t $ 19 * s
	endfill t
