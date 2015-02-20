module Machines (
	Value(..),
	putMachine, putOperator, putValue, arrow, bigArrow) where

import Control.Monad
import Text.XML.YJSVG hiding (topleft)
import Graphics.X11.Turtle

putOperator :: Turtle -> Bool -> Value -> Value -> Value -> Double -> IO ()
putOperator t ar v1 v2 vo s = do
	(x, y) <- position t
	goto t (x + 22 * s) (y + 5 * s)
	setheading t 0
	beginfill t
	replicateM_ 36 $ forward t (pi * s / 2) >> right t 10
	endfill t
	goto t (x + 10 * s) (y + 5 * s)
	beginfill t
	setheading t (- 30)
	replicateM_ 2 $
		forward t (8 * s) >> right t 90 >> forward t (4 * s) >> right t 90
	endfill t
	goto t (x + 10 * s) (y + 25 * s)
	setheading t 30
	beginfill t
	replicateM_ 2 $
		forward t (8 * s) >> left t 90 >> forward t (4 * s) >> left t 90
	endfill t
	return ()
	goto t (x + 30 * s) (y + 12 * s)
	setheading t 15
	beginfill t
	forward t (8 * s)
	right t 105
	forward t (10 * s)
	right t 105
	forward t (8 * s)
	endfill t

	when ar $ do
		goto t (x + s) (y + 2.4 * s)
		setheading t (- 30)
		arrow t 7 s
		goto t (x + s) (y + 27.6 * s)
		setheading t 30
		arrow t 7 s
		goto t (x + 40 * s) (y + 15 * s)
		setheading t 0
		arrow t 7 s
		return ()

	pencolor t "red"
	goto t (x + 16 * s) (y + 10.5 * s)
	putValue t v1 1
	goto t (x + 16 * s) (y + 17 * s)
	putValue t v2 1
	goto t (x + 28 * s) ( y + 14 * s)
	putValue t vo 1

putMachine :: Turtle -> Bool -> [Value] -> Maybe Value -> Double -> IO ()
putMachine t ar vs v s = do
	penup t
	(x, y) <- position t
	goto t x (y + 8 * s)
	setheading t 0
	beginfill t
	replicateM_ 2 $
		forward t (20 * s) >> right t 90 >> forward t (15 * s) >> right t 90
	endfill t
	goto t (x + 3 * s) y
	beginfill t
	replicateM_ 2 $
		forward t (4 * s) >> right t 90 >> forward t (8 * s) >> right t 90
	endfill t
	goto t (x + 13 * s) (y + 8 * s)
	setheading t 90
	beginfill t
	forward t (5 * s)
	right t 45
	forward t (4 * s)
	right t 45
	forward t (4 * s)
	right t 45
	forward t (10 * s)
	right t 90
	forward t (4 * s)
	right t 90
	forward t (9 * s)
	left t 45
	forward t $ 0.7 * s
	left t 90
	forward t (5 * s)
	endfill t

	when ar $ do
		goto t (x + 27 * s) (y + 10 * s)
		setheading t (- 45)
		arrow t 7 s
		goto t (x + 5 * s) (y - 8 * s)
		setheading t (- 90)
		arrow t 7 s
		penup t

	pencolor t "red"
	goto t (x + 4 * s) (y + 10 * s)
	setheading t 0
	forM_ vs $ \v -> do
		(x, y) <- position t
		putValue t v (s / 4)
		goto t (x + 2 * s) y
	goto t (x + 14 * s) (y + 10 * s)
	maybe (return ()) (flip (putValue t) (s / 4)) v

arrow :: Turtle -> Double -> Double -> IO ()
arrow t l s = do
	pensize t s
	pendown t
	forward t $ l * s
	penup t
	left t 30
	backward t $ 2 * s
	pendown t
	forward t $ 2 * s
	right t 60
	backward t $ 2 * s
	penup t

data Value = ValueA | ValueB | ValueC | ValueD | Machine deriving Show

putValue :: Turtle -> Value -> Double -> IO ()
putValue t ValueA s = putValueA t s
putValue t ValueB s = putValueB t s
putValue t ValueC s = putValueC t s
putValue t ValueD s = putValueD t s
putValue t Machine s = do
	(x, y) <- position t
	goto t x (y - 2 * s)
	putMachine t False [] Nothing (s / 2)

putValueA, putValueB, putValueC, putValueD :: Turtle -> Double -> IO ()
putValueA t s = do
	setheading t 0
	forward t $ 4 * s
	beginfill t
	replicateM_ 36 $ forward t (pi * s * 2 / 9) >> right t 10
	endfill t
putValueB t s = do
	setheading t 0
	forward t $ 4 * s
	setheading t (- 60)
	beginfill t
	replicateM_ 3 $ forward t (8 * s) >> right t 120
	endfill t
putValueC t s = do
	setheading t (- 90)
	forward t $ 0.5 * s
	setheading t 0
	forward t $ 0.5 * s
	beginfill t
	replicateM_ 4 $ forward t (7 * s) >> right t 90
	endfill t
putValueD t s = do
	setheading t (- 90)
	forward t s
	setheading t 0
	forward t $ 8 / 3 * s
	beginfill t
	replicateM_ 36 $ forward t (pi * s * 4 / 27) >> right t 10
	endfill t
	forward t $ 14 / 3 * s
	beginfill t
	replicateM_ 36 $ forward t (pi * s * 4 / 27) >> right t 10
	endfill t

bigArrow :: Turtle -> Double -> Double -> IO ()
bigArrow t l s = do
	pensize t (8 * s)
	pendown t
	forward t (l * s)
	penup t
	left t 30
	backward t (8 * s)
	pendown t
	forward t (8 * s)
	right t 60
	backward t (8 * s)
	penup t
