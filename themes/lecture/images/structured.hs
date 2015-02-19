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
	goto t 20 20
	jun t 16
	goto t 110 20
	han t 16
	goto t 200 20
	bun t 16
	hideturtle t
	svg <- getSVG t
	writeFile "structured.svg" $ showSVG 255 100 svg
	waitField f

jun :: Turtle -> Double -> IO ()
jun t s = do
	pensize t s
	(x, y) <- position t
	pendown t
	setheading t (- 45)
	forward t $ 2.5 * s
	right t 90
	forward t $ 2.5 * s
	penup t
	goto t (x + 2 * s) y
	pendown t
	setheading t (- 45)
	forward t $ 2.5 * s
	right t 90
	forward t $ 2.5 * s
	penup t

han :: Turtle -> Double -> IO ()
han t s = do
	(x, y) <- position t
	pensize t s
	goto t x (y + 2 * s)
	setheading t (- 90)
	pendown t
	replicateM_ 32 $ forward t (s / 3) >> left t 10
	right t 20
	arrow t s
	penup t

bun :: Turtle -> Double -> IO ()
bun t s = do
	(x, y) <- position t
	pensize t s
	goto t x (y + 2 * s)
	pendown t
	setheading t 35
	forward t $ 3.5 * s
	arrow t s
	penup t
	goto t x (y + 2 * s)
	pendown t
	setheading t (- 35)
	forward t $ 3.5 * s
	arrow t s
	penup t

arrow :: Turtle -> Double -> IO ()
arrow t s = do
	pensize t s
	left t 45
	backward t $ 1 * s
	pendown t
	forward t $ 1 * s
	right t 90
	backward t $ 1 * s
	penup t
