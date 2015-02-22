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

	goto t 18 15
	putValue t Machine 4
	pencolor t "black"
	goto t 15 90
	putMachine t True [Machine] (Just Machine) 4
	pencolor t "black"

	goto t 168 20
	putValue t ValueA 4
	goto t 165 90
	putMachine t True [ValueA] (Just ValueB) 4
	pencolor t "black"

	goto t 294 155
	putValue t ValueB 4

	hideturtle t
	svg <- getSVG t
	writeFile "function_apply.svg" $ showSVG 330 190 svg
	waitField f
