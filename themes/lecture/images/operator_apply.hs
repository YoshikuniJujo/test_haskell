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

	goto t 22 15
	putValue t Machine 4
	pencolor t "black"
	goto t 28 139
	putValue t ValueA 4
	goto t 70 33
	putOperator t True Machine ValueA ValueB 4
	pencolor t "black"
	goto t 263 79
	putValue t ValueB 4

	hideturtle t
	svg <- getSVG t
	writeFile "operator_apply.svg" $ showSVG 310 175 svg
	waitField f
