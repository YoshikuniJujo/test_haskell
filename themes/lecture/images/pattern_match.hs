import Control.Monad
import Text.XML.YJSVG hiding (topleft)
import Graphics.X11.Turtle

import Machines

main :: IO ()
main = do
	f <- openField
	topleft f
	onkeypress f $ return . (/= 'q')
	t <- newTurtle f
	penup t

	goto t 170 5
	putGate t "black" "red" PatternB 4

	goto t 10 55
	putAlgData t "black" "red" PatternA (Just ValueA) 4

	goto t 90 75
	setheading t 0
	arrow t 15 4

	goto t 170 55
	putGate t "black" "red" PatternA 4

	pencolor t "black"
	goto t 230 75
	setheading t 0
	arrow t 15 4

	goto t 300 60
	putValue t ValueA 4

	goto t 170 105
	putGate t "black" "red" PatternC 4

	hideturtle t
	svg <- getSVG t
	writeFile "pattern_match.svg" $ showSVG 360 147 svg
	waitField f

putAlgData :: (ColorClass c1, ColorClass c2) =>
	Turtle -> c1 -> c2 -> Pattern -> Maybe Value -> Double -> IO ()
putAlgData t c cp p v s = do
	(x, y) <- position t
	pencolor t c
	goto t x (y + 2 * s)
	penup t
	setheading t 0
	beginfill t
	replicateM_ 2 $
		forward t (8 * s) >> right t 90 >> forward t (6 * s) >> right t 90
	endfill t
	goto t (x + 9 * s) y
	beginfill t
	replicateM_ 2 $
		forward t (6 * s) >> right t 90 >> forward t (8 * s) >> right t 90
	endfill t
	goto t (x + 3 * s) (y + 8 * s)
	beginfill t
	replicateM_ 36 $ forward t (pi * 0.1 * s) >> right t 10
	endfill t
	goto t (x + 12 * s) (y + 8 * s)
	beginfill t
	replicateM_ 36 $ forward t (pi * 0.1 * s) >> right t 10
	endfill t
	goto t (x + 13.2 * s) (y - 1.8 * s)
	putPattern t cp p $ s / 3
	goto t (x + 1.2 * s) (y - 3.7 * s)
	pencolor t c
	maybe (return ()) (flip (putValue t) $ 0.7 * s) v

putGate :: (ColorClass c1, ColorClass c2) =>
	Turtle -> c1 -> c2 -> Pattern -> Double -> IO ()
putGate t c cp p s = do
	(x, y) <- position t
	pencolor t c
	goto t x (y + 10 * s)
	setheading t 0
	beginfill t
	forward t $ 2 * s
	left t 90
	forward t $ 7 * s
	right t 90
	forward t $ 5 * s
	right t 90
	forward t $ 7 * s
	left t 90
	forward t $ 2 * s
	left t 90
	forward t $ 9 * s
	left t 90
	forward t $ 9 * s
	endfill t
	goto t (x + 7 * s) (y - 1 * s)
	putPattern t cp p $ s / 3

data Pattern = PatternA | PatternB | PatternC deriving Show

putPattern :: ColorClass c => Turtle -> c -> Pattern -> Double -> IO ()
putPattern t c PatternA s = do
	(x, y) <- position t
	pencolor t c
	penup t
	goto t (x + 5 * s) y
	setheading t (- 72)
	beginfill t
	replicateM_ 5 $ do
		forward t $ 4 * s
		left t 72
		forward t $ 4 * s
		right t 144
	endfill t
putPattern t c PatternB s = do
	(x, y) <- position t
	pencolor t c
	goto t (x + 7 * s) (y + 0.5 * s)
	setheading t (- 45)
	beginfill t
	replicateM_ 19 $ forward t (pi * 0.28 * s) >> right t 10
	left t 45
	replicateM_ 12 $ backward t (pi * 1.15 * 0.28 * s) >> left t 10
	endfill t
putPattern t c PatternC s = do
	(x, y) <- position t
	pencolor t c
	goto t (x + 6 * s) (y + 2.5 * s)
	setheading t 0
	beginfill t
	replicateM_ 36 $ forward t (pi * 0.13 * s) >> right t 10
	endfill t
	pensize t s
	goto t (x + 6 * s) (y + 2.5 * s)
	replicateM_ 9 $ do
		left t 90
		forward t $ 1.2 * s
		pendown t
		forward t $ 0.8 * s
		penup t
		backward t $ 2 * s
		right t 90
		replicateM_ 4 $ forward t (pi * 0.13 * s) >> right t 10
	return ()
