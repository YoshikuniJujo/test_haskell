import Control.Monad
import Text.XML.YJSVG hiding (topleft)
import Graphics.X11.Turtle

import Machine2

main, main1, main2 :: IO ()
main = main1 >> main2
main1 = do
	f <- openField
	topleft f
	onkeypress f $ return . (/= 'q')
	t <- newTurtle f
	penup t

	goto t 20 5
	half t "black" 4

	goto t 111 9
	convertor t "black" 4

	goto t 150 9
	seven t "black" 4

	hideturtle t
	svg <- getSVG t
	writeFile "conversion_adapter1.svg" $ showSVG 220 85 svg
main2 = do
	f <- openField
	topleft f
	onkeypress f $ return . (/= 'q')
	t <- newTurtle f
	penup t

	goto t 20 5
	half t "black" 4

	goto t 70 9
	convertor t "black" 4

	goto t 150 9
	seven t "black" 4

	hideturtle t
	svg <- getSVG t
	writeFile "conversion_adapter2.svg" $ showSVG 220 85 svg
	waitField f

half :: ColorClass c => Turtle -> c -> Double -> IO ()
half t c s = machine t c armA s

convertor :: ColorClass c => Turtle -> c -> Double -> IO ()
convertor t c s = do
	penup t
	pencolor t c
	setheading t 0
	forward t $ 2 * s
	beginfill t
	forward t $ 6 * s
	setheading t (- 150)
	replicateM_ 13 $ forward t (pi / 5.5 * s) >> left t 10
	setheading t 0
	backward t $ 6 * s
	setheading t 90
	forward t $ 1.4 * s
	left t 90
	forward t $ 2 * s
	right t 90
	forward t $ 3 * s
	right t 90
	forward t $ 2 * s
	left t 90
	forward t s
	endfill t

seven :: ColorClass c => Turtle -> c -> Double -> IO ()
seven t c s = do
	(x, y) <- position t
	penup t
	goto t x y
	pencolor t c
	setheading t 0
	beginfill t
	replicateM_ 36 $ forward t (pi / 6 * s) >> right t 10
	endfill t
