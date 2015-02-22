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

	goto t 13 15
	putValue t Machine 4
	pencolor t "black"
	goto t 10 90
	putMachine t True [Machine] (Just Machine) 4
	pencolor t "black"

	goto t 163 15
	putValue t Machine 4
	pencolor t "black"
	goto t 160 90
	putMachine t True [Machine] (Just Machine) 4
	pencolor t "black"

	goto t 313 20
	putValue t ValueA 4
	pencolor t "black"
	goto t 310 90
	putMachine t True [ValueA] (Just ValueC) 4
	pencolor t "black"

	goto t 441 152
	putValue t ValueC 4

	hideturtle t
	svg <- getSVG t
	writeFile "function_dot.svg" $ showSVG 475 190 svg
	waitField f
