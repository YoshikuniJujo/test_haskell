import Control.Monad
import Text.XML.YJSVG hiding (topleft)
import Graphics.X11.Turtle

main :: IO ()
main = do
	f <- openField
	topleft f
	onkeypress f $ return . (/= 'q')
	t <- newTurtle f
	penup t

	goto t 20 5
	beginfill t
	replicateM_ 2 $ do
		forward t 200
		right t 90
		forward t 20
		right t 90
	endfill t

	goto t 20 30
	setheading t 0
	beginfill t
	forward t 250
	right t 90
	forward t 20
	right t 90
	forward t 100
	left t 90
	forward t 20
	left t 90
	forward t 50
	right t 90
	forward t 20
	right t 90
	forward t 150
	right t 90
	forward t 40
	left t 90
	forward t 50
	endfill t

	goto t 20 95
	setheading t 0
	beginfill t
	forward t 150
	right t 90
	forward t 20
	left t 90
	forward t 100
	right t 90
	forward t 20
	right t 90
	forward t 200
	right t 90
	forward t 20
	left t 90
	forward t 50
	endfill t

	hideturtle t
	svg <- getSVG t
	writeFile "layout_rule_0.svg" $ showSVG 280 140 svg
	waitField f
