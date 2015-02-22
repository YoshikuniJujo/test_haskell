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

	goto t 20 15
	putValue t Machine 4
	pencolor t "black"
	goto t 20 125
	putValue t Machine 4
	goto t 70 33
	pencolor t "black"
	putOperator t True Machine Machine Machine 4
	pencolor t "black"
	goto t 267 76
	putValue t Machine 4

	hideturtle t
	svg <- getSVG t
	writeFile "operator_dot.svg" $ showSVG 325 175 svg
	waitField f
