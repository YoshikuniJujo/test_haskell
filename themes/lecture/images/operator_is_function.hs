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

	goto t 10 25
	openParens t 4

	goto t 20 0
	putOperator t False ValueA ValueB ValueC 4

	pencolor t "black"
	goto t 190 25
	closeParens t 4

	goto t 235 60
	setheading t 0
	bigArrow t 14 4

	goto t 320 10
	putMachine t False [ValueA] (Just Machine) 4

	hideturtle t
	svg <- getSVG t
	writeFile "operator_is_function.svg" $ showSVG 430 110 svg
	waitField f

openParens, closeParens :: Turtle -> Double -> IO ()
openParens t s = do
	(x, y) <- position t
	goto t (x + 4 * s) y
	setheading t (- 135)
	pensize t (s * 2)
	pendown t
	replicateM_ 10 $ forward t (s * 2) >> left t 10
	penup t
closeParens t s = do
	(x, y) <- position t
	goto t x y
	setheading t (- 45)
	pensize t (s * 2)
	pendown t
	replicateM_ 10 $ forward t (s * 2) >> right t 10
	penup t
