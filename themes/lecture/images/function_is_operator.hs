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

	goto t 5 5
	backQuote t 4

	goto t 50 20
	putMachine t False [ValueA] (Just Machine) 4

	pencolor t "black"
	goto t 150 5
	backQuote t 4

	goto t 205 70
	setheading t 0
	bigArrow t 15 4

	goto t 265 10
	putOperator t False ValueA ValueB ValueC 4

	hideturtle t
	svg <- getSVG t
	writeFile "function_is_operator.svg" $ showSVG 435 110 svg
	waitField f

backQuote :: Turtle -> Double -> IO ()
backQuote t s = do
	(x, y) <- position t
	penup t
	goto t (x + 7 * s) (y + 10 * s)
	setheading t 120
	beginfill t
	forward t $ 10 * s
	left t 110
	forward t $ 3 * s
	endfill t
