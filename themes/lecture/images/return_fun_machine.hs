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

	goto t 20 5
	putMachine t False [ValueA] (Just Machine) 4

	hideturtle t
	svg <- getSVG t
	writeFile "return_fun_machine.svg" $ showSVG 150 105 svg
	waitField f
