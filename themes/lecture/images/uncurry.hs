import Control.Monad
import Text.XML.YJSVG hiding (topleft)
import Graphics.X11.Turtle

main :: IO ()
main = do
	f <- openField
	topleft f
	onkeypress f $ return . (/= 'q')
	t <- newTurtle f
	penup t

	goto t 70 5
	putUncurry t "black" Take 4

	goto t 50 125
	twoArgFun t "black" 4

	goto t 172 105
	tup t "black" 4

	hideturtle t
	svg <- getSVG t
	writeFile "uncurry.svg" $ showSVG 250 190 svg
	waitField f

data UncurryState = Take | Give deriving Show

putUncurry :: ColorClass c => Turtle -> c -> UncurryState -> Double -> IO ()
putUncurry t c us s = do
	(x, y) <- position t
	penup t
	pencolor t c
	setheading t 0
	beginfill t
	replicateM_ 2 $
		forward t (30 * s) >> right t 90 >> forward t (10 * s) >> right t 90
	endfill t
	goto t (x + 10 * s) (y + 10 * s)
	arm2 t c s

arm1, arm2, arm3 :: ColorClass c => Turtle -> c -> Double -> IO ()
arm1 t c s = return ()
arm2 t c s = do
	(x, y) <- position t
	penup t
	goto t (x + 8.9 * s) (y + 22.9 * s)
	setheading t 90
	beginfill t
	forward t $ 10 * s
	right t 90
	forward t $ 6 * s
	right t 90
	forward t $ 1.9 * s
	right t 60
	replicateM_ 13 $ forward t (pi / 5.2 * s) >> left t 10
	right t 70
	forward t $ 1.9 * s
	endfill t

	goto t (x - 21 * s) (y + 24 * s)
	setheading t 0
	beginfill t
	forward t $ 5 * s
	right t 90
	forward t $ 5 * s
	left t 90
	forward t $ 5 * s
	right t 90
	forward t $ 5 * s
	right t 90
	forward t $ 10 * s
	endfill t

	goto t (x - 21 * s) (y + 24 * s)
	setheading t 0
	beginfill t
	forward t $ 3 * s
	left t 90
	forward t $ 28 * s
	right t 90
	forward t $ 8 * s
	left t 90
	forward t $ 3 * s
	left t 90
	forward t $ 11 * s
	endfill t

	goto t (x + 10 * s) (y + 13 * s)
	setheading t 120
	beginfill t
	forward t $ 15.1 * s
	right t 120
	forward t $ 4 * s
	right t 60
	forward t $ 15.1 * s
	endfill t

	goto t (x + 30 * s) (y + 14.2 * s)
	setheading t (- 120)
	beginfill t
	forward t $ 6 * s
	right t 90
	forward t $ 3 * s

	right t 60
	forward t $ 4 * s
	left t 120
	forward t $ 4 * s
	right t 60
--	forward t $ 6 * s

	forward t $ 3 * s
	right t 90
	forward t $ 6 * s
	endfill t

	goto t (x + 19 * s) (y + 13.5 * s)
	setheading t 120
	beginfill t
	forward t $ 15.6 * s
	right t 120
	forward t $ 4 * s
	right t 60
	forward t $ 11.6 * s
	endfill t

arm3 t c s = return ()

twoArgFun, armA, armB :: ColorClass c => Turtle -> c -> Double -> IO ()
twoArgFun t c s = do
	(x, y) <- position t
	pencolor t c
	setheading t 0
	beginfill t
	replicateM_ 2 $
		forward t (20 * s) >> right t 90 >> forward t (8 * s) >> right t 90
	endfill t
	goto t (x + 3 * s) y
	armA t c s
	goto t (x + 14 * s) y
	armB t c s
armA t c s = do
	pencolor t c
	setheading t 90
	beginfill t
	forward t $ 2 * s
	left t 90
	forward t $ 3 * s
	right t 90
	forward t $ 5 * s
	right t 90
	forward t $ 2 * s

	setheading t (- 60)
	replicateM_ 13 $ forward t (pi / 5.2 * s) >> left t 10

	setheading t 0
	forward t $ 2 * s
	right t 90
	forward t $ 5 * s
	right t 90
	forward t $ 3 * s
	left t 90
	forward t $ 2 * s
	endfill t
armB t c s = do
	pencolor t c
	setheading t 90
	beginfill t
	forward t $ 2 * s
	left t 90
	forward t $ 3 * s
	right t 90
	forward t $ 5 * s
	right t 90
	forward t $ 3 * s

	setheading t (- 60)
	forward t $ 4 * s
	left t 120
	forward t $ 4 * s

	setheading t 0
	forward t $ 3 * s
	right t 90
	forward t $ 5 * s
	right t 90
	forward t $ 3 * s
	left t 90
	forward t $ 2 * s
	endfill t

tup, valueA, valueB :: ColorClass c => Turtle -> c -> Double -> IO ()
tup t c s = do
	(x, y) <- position t
	valueA t c s
	goto t (x + 3 * s) y
	setheading t 30
	valueB t c s
valueA t c s = do
	(x, y) <- position t
	goto t (x + s) y
	setheading t 0
	beginfill t
	replicateM_ 36 $ forward t (pi / 6 * s) >> right t 10
	endfill t
valueB t c s = do
	(x, y) <- position t
	goto t x (y + 3 * s)
	beginfill t
	replicateM_ 3 $ forward t (6 * s) >> right t 120
	endfill t
