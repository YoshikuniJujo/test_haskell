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
	putMachine t True [ValueA] (Just ValueB) 4
	goto t 150 140
	pencolor t "black"
	putValue t ValueB 4

	hideturtle t
	svg <- getSVG t
	writeFile "simple_function.svg" $ showSVG 200 175 svg
	waitField f
