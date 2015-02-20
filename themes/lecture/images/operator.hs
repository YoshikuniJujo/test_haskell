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

	goto t 22 5
	putValue t ValueA 4
	goto t 25 126
	putValue t ValueB 4
	goto t 60 23
	putOperator t True ValueA ValueB ValueC 4
	pencolor t "black"
	goto t 260 66
	putValue t ValueC 4

	hideturtle t
	svg <- getSVG t
	writeFile "operator.svg" $ showSVG 310 160 svg
	waitField f
