module BBTree (
	value, box, long, tree, emptyTree, arrow,
	label, labeledTree, labeledValue, labeledBox, labeledLong) where 
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

tree :: Turtle -> Double -> IO ()
tree t s = do
	(x0, y0) <- position t
	box t s
	goto t (x0 + 5 * s) (y0 + 2 * s)
	pendown t
	beginfill t
	setheading t (- 120)
	replicateM_ 3 $ forward t (7 * s) >> left t 120
	endfill t
	penup t

box :: Turtle -> Double -> IO ()
box t s = do
	(x0, y0) <- position t
	setheading t (- 90)
	pendown t
	replicateM_ 4 $ forward t (10 * s) >> left t 90
	penup t

long :: Turtle -> Double -> IO ()
long t s = do
	(x0, y0) <- position t
	setheading t (- 90)
	pendown t
	replicateM_ 2 $ do
		forward t (10 * s) >> left t 90
		forward t (16 * s) >> left t 90
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

label :: Turtle -> String -> Double -> IO ()
label t l s = do
	pencolor t "black"
	write t "KochiGothic" (s * 15 / 2) l
	pencolor t "gray"

labeledTree :: Turtle -> String -> Double -> IO ()
labeledTree t l s = do
	(x0, y0) <- position t
	tree t s
	goto t (x0 + 13 / 4 * s) (y0 + 7 * s)
	label t l s

labeledValue :: Turtle -> String -> Double -> IO ()
labeledValue t l s = do
	(x0, y0) <- position t
	value t s
	goto t (x0 - 2 * s) (y0 - 2 * s)
	label t l s

labeledBox :: Turtle -> String -> Double -> IO ()
labeledBox t l s = do
	(x0, y0) <- position t
	box t (3 * s)
	goto t (x0 + s) (y0 + 8 * s)
	label t l s

labeledLong :: Turtle -> String -> Double -> IO ()
labeledLong t l s = do
	(x0, y0) <- position t
	long t (3 * s)
	goto t (x0 + s) (y0 + 8 * s)
	label t l s
