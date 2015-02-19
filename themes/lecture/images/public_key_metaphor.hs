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
	goto t 60 20
	king t 4
	goto t 160 90
	minister t 4
	goto t 260 20
	citizen t 4
	setheading t 180
	goto t 235 50
	pendown t
	pensize t 4
	pencolor t "black"
	forward t 100
	arrow t 4
	penup t
	goto t 170 70
	closed t 4
	goto t 10 70
	opened t 4
	goto t 310 35
	ckey t 4
	goto t 210 110
	ckey t 4
	goto t 40 35
	okey t 4
	hideturtle t
	svg <- getSVG t
	writeFile "public_key_metaphor.svg" $ showSVG 330 145 svg
	waitField f

king :: Turtle -> Double -> IO ()
king t s = do
	(x, y) <- position t
	crown t s
	goto t (x + s / 2) (y + 10 * s)
	hige t $ s / 2

minister :: Turtle -> Double -> IO ()
minister t s = do
	(x, y) <- position t
	hat t True s
	goto t (x + 1.7 * s) (y + 10 * s)
	hige t $ s / 2.5

citizen :: Turtle -> Double -> IO ()
citizen t s = do
	hat t False s

crown :: Turtle -> Double -> IO ()
crown t s = do
	(x, y) <- position t
	pensize t s
	pencolor t (0xcf, 0xcf, 0x00)
	goto t x (y + 0 * s)
	setheading t (- 90)
	beginfill t
	forward t $ 6 * s
	left t 90
	forward t $ 10 * s
	left t 90
	forward t $ 6 * s
	left t 150
	forward t $ 5 * s
	right t 120
	forward t $ 5 * s
	left t 120
	forward t $ 5 * s
	right t 120
	forward t $ 5 * s
	endfill t

hige :: Turtle -> Double -> IO ()
hige t s = do
	(x, y) <- position t
	goto t x (y + 2 * s)
	pencolor t "black"
	beginfill t
	setheading t 30
	forward t $ 3 * s
	left t 20
	forward t $ 3 * s
	right t 50
	forward t $ 3 * s
	right t 90
	forward t $ 3 * s
	endfill t
	goto t (x + 18 * s) (y + 2 * s)
	beginfill t
	setheading t 150
	forward t $ 3 * s
	right t 20
	forward t $ 3 * s
	left t 50
	forward t $ 3 * s
	left t 90
	forward t $ 3 * s
	endfill t

hat :: Turtle -> Bool -> Double -> IO ()
hat t b s = do
	(x, y) <- position t
	pencolor t $ if b then "black" else "brown"
	goto t x (y + 8 * s)
	setheading t 0
	beginfill t
	forward t $ 11 * s
	left t 160
	forward t $ 3 * s
	right t 70
	replicateM_ 19 $ forward t (s / 2) >> left t 10
	endfill t

arrow :: Turtle -> Double -> IO ()
arrow t s = do
	penup t
	pensize t s
	left t 30
	backward t $ 2 * s
	pendown t
	forward t $ 2 * s
	right t 60
	backward t $ 2 * s
	penup t

closed :: Turtle -> Double -> IO ()
closed t s = do
	(x, y) <- position t
	goto t x (y + 1.5 * s)
	box t s
	goto t x (y + 1 * s)
	setheading t 0
	cover t s

opened :: Turtle -> Double -> IO ()
opened t s = do
	(x, y) <- position t
	goto t x (y + 1.5 * s)
	box t s
	goto t x (y + 1 * s)
	setheading t 30
	cover t s

box :: Turtle -> Double -> IO ()
box t s = do
	penup t
	pencolor t "brown"
	setheading t 0
	beginfill t
	replicateM_ 2 $
		forward t (8 * s) >> right t 90 >> forward t (3 * s) >> right t 90
	endfill t

cover :: Turtle -> Double -> IO ()
cover t s = do
	beginfill t
	forward t $ 8 * s
	left t 120
	forward t $ 2 * s
	left t 60
	forward t $ 5.5 * s
	endfill t

ckey :: Turtle -> Double -> IO ()
ckey t s = do
	(x, y) <- position t
	pencolor t "gray"
	goto t x (y + 2 * s)
	setheading t 135
	beginfill t
	replicateM_ 7 $ forward t s >> right t 45
	left t 90
	forward t $ 2 * s
	left t 90
	replicateM_ 2 $ forward t s >> right t 90
	forward t s
	left t 90
	forward t s
	right t 90
	forward t s
	endfill t

okey :: Turtle -> Double -> IO ()
okey t s = do
	(x, y) <- position t
	pencolor t "gray"
	goto t x (y + 2 * s)
	setheading t 135
	beginfill t
	replicateM_ 7 $ forward t s >> right t 45
	left t 90
	forward t s
	left t 90
	forward t s
	right t 90
	forward t $ 2 * s
	right t 90
	forward t s
	left t 90
	forward t $ 1 * s
	right t 90
	forward t s
	endfill t
