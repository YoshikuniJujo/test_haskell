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

	goto t 75 15
	whole t "black" 4

	goto t 75 60
	setheading t (- 135)
	arrow t "black" 8 4
	setheading t 0
	goto t 20 90
	partsA t "black" 4

	goto t 120 60
	setheading t (- 90)
	arrow t "black" 6 4
	setheading t 0
	goto t 95 90
	partsB t "black" 4

	goto t 165 60
	setheading t (- 45)
	arrow t "black" 8 4
	setheading t 0
	goto t 195 90
	partsC t "black" 4

	hideturtle t
	svg <- getSVG t
	writeFile "topdown.svg" $ showSVG 230 135 svg
	waitField f

whole :: ColorClass c => Turtle -> c -> Double -> IO ()
whole t c s = do
	(x, y) <- position t
	setheading t 0
	partsA t c s
	goto t (x + 5 * s) (y - s)
	setheading t 0
	partsB t c s
	goto t (x + 14 * s) (y - 2 * s)
	setheading t 0
	partsC t c s

partsA :: ColorClass c => Turtle -> c -> Double -> IO ()
partsA t c s = do
	penup t
	pencolor t c
	beginfill t
	forward t $ 4 * s
	right t 90
	forward t $ 4 * s
	left t 90
	forward t $ 4 * s
	right t 90
	forward t $ 4 * s
	right t 90
	forward t $ 8 * s
	endfill t

partsB :: ColorClass c => Turtle -> c -> Double -> IO ()
partsB t c s = do
	penup t
	pencolor t c
	beginfill t
	forward t $ 8 * s
	right t 90
	forward t $ 4 * s
	left t 90
	forward t $ 4 * s
	right t 90
	forward t $ 4 * s
	right t 90
	forward t $ 8 * s
	right t 90
	forward t $ 4 * s
	left t 90
	forward t $ 4 * s
	endfill t

partsC :: ColorClass c => Turtle -> c -> Double -> IO ()
partsC t c s = do
	penup t
	pencolor t c
	beginfill t
	replicateM_ 4 $ forward t (4 * s) >> right t 90
	endfill t

arrow :: ColorClass c => Turtle -> c -> Double -> Double -> IO ()
arrow t c l s = do
	pencolor t c
	pensize t s
	pendown t
	forward t $ l * s
	left t 30
	penup t
	backward t $ 2 * s
	pendown t
	forward t $ 2 * s
	right t 60
	backward t $ 2 * s
	penup t
