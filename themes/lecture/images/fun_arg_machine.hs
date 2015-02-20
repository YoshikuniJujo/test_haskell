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

	goto t 23 15
	putValue t Machine 4
	pencolor t "black"
	goto t 20 92
	putMachine t True [Machine] (Just Machine) 4

	pencolor t "black"
	goto t 168 20
	putValue t ValueA 4
	pencolor t "black"
	goto t 165 92
	putMachine t True [ValueA] (Just ValueB) 4

	goto t 295 155
	pencolor t "black"
	putValue t ValueB 4

	hideturtle t
	svg <- getSVG t
	writeFile "fun_arg_machine.svg" $ showSVG 345 200 svg
	waitField f
