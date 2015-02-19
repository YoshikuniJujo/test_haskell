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
	pensize t 8
	goto t 30 45
	pendown t
	forward t 80
	penup t
	goto t 120 10
	pendown t
	beginfill t
	replicateM_ 2 $ forward t 80 >> right t 90 >> forward t 70 >> right t 90
	endfill t
	penup t
	goto t 210 45
	pendown t
	forward t 80
	penup t
	right t 30
	backward t 10
	pendown t
	forward t 10
	left t 60
	backward t 10
	hideturtle t
	svg <- getSVG t
	writeFile "black_box.svg" $ showSVG 330 95 svg
	waitField f
