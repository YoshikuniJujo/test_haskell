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

	goto t 18 5
	putValue t ValueA 4
	goto t 15 75
	putMachine t True [ValueA] (Just Machine) 4
	pencolor t "black"

	goto t 170 9
	putValue t ValueB 4
	goto t 165 75
	putMachine t True [ValueB] (Just ValueA) 4
	pencolor t "black"

	goto t 292 137
	putValue t ValueA 4

	hideturtle t
	svg <- getSVG t
	writeFile "function_const.svg" $ showSVG 330 175 svg
	waitField f
