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

	goto t 23 5
	putValue t ValueA 4
	goto t 20 75
	putMachine t True [ValueA] (Just Machine) 4

	pencolor t "black"
	goto t 175 9
	putValue t ValueB 4
	goto t 170 75
	putMachine t True [ValueB] (Just ValueC) 4

	pencolor t "black"
	goto t 310 140
	putValue t ValueC 4

	hideturtle t
	svg <- getSVG t
	writeFile "two_arg_fun.svg" $ showSVG 340 175 svg
	waitField f
