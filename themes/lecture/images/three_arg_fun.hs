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

	pencolor t "black"
	goto t 13 10
	putValue t ValueA 4
	goto t 10 80
	putMachine t True [ValueA] (Just Machine) 4

	pencolor t "black"
	goto t 160 12
	putValue t ValueB 4
	goto t 155 80
	putMachine t True [ValueB] (Just Machine) 4

	pencolor t "black"
	goto t 304 11
	putValue t ValueC 4
	goto t 300 80
	putMachine t True [ValueC] (Just ValueD) 4
	pencolor t "black"
	goto t 430 150
	putValue t ValueD 4

	hideturtle t
	svg <- getSVG t
	writeFile "three_arg_fun.svg" $ showSVG 480 190 svg
	waitField f
