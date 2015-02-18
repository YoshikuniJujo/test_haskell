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
	pensize t 5
	goto t 10 10
	pendown t
	replicateM_ 2 $ forward t 460 >> right t 90 >> forward t 100 >> right t 90
	penup t
	goto t 30 30
	the t 5
	goto t 100 40
	putDot t 5
	goto t 130 23
	kan t 5
	goto t 200 23
	suu t 5
	goto t 258 23
	gata t 5
	goto t 320 23
	gen t 5
	goto t 380 23
	go t 5
	hideturtle t
	svg <- getSVG t
	writeFile "the_functional_language.svg" $ showSVG 480 115 svg
	waitField f

the :: Turtle -> Double -> IO ()
the t s = do
	(x, y) <- position t
	pensize t s
	penup t
	setheading t 0
	right t 90
	forward t $ 3 * s
	left t 90
	pendown t
	forward t $ 10 * s
	penup t
	goto t (x + 2 * s) y
	pendown t
	setheading t (- 90)
	forward t $ 5 * s
	penup t
	goto t (x + 7 * s) y
	pendown t
	forward t $ 8 * s
	right t 30
	forward t $ 5 * s
	penup t
	goto t (x + 10 * s) y
	pendown t
	setheading t (- 45)
	forward t $ 2 * s
	penup t
	goto t (x + 12 * s) (y - 2 * s)
	pendown t
	setheading t (- 45)
	forward t $ 2 * s
	penup t

putDot :: Turtle -> Double -> IO ()
putDot t s = do
	pensize t 1
	forward t 0
	penup t
	forward t $ 2 * s
	beginfill t
	replicateM_ 36 $ forward t (0.2 * s) >> right t 10
	endfill t

kan :: Turtle -> Double -> IO ()
kan t s = do
	(x, y) <- position t
	pensize t s
	penup t
	setheading t (- 90)
	forward t $ 14 * s
	setheading t 90
	pendown t
	forward t $ 14 * s
	right t 90
	forward t $ 4 * s
	right t 90
	forward t $ 5 * s
	right t 90
	forward t $ 4 * s
	right t 90
	penup t
	forward t $ 2.5 * s
	right t 90
	pendown t
	forward t $ 4 * s
	penup t
	goto t (x + 11 * s) (y + 14 * s)
	pendown t
	setheading t 90
	forward t $ 14 * s
	left t 90
	forward t $ 4 * s
	left t 90
	forward t $ 5 * s
	left t 90
	forward t $ 4 * s
	left t 90
	penup t
	forward t $ 2.5 * s
	left t 90
	pendown t
	forward t $ 4 * s
	penup t
	goto t (x + 2.8 * s) (y + 6.5 * s)
	pendown t
	setheading t (- 45)
	forward t $ 1.5 * s
	penup t
	goto t (x + 8.2 * s) (y + 6.5 * s)
	pendown t
	setheading t (- 135)
	forward t $ 1.5 * s
	penup t
	goto t (x + 3.2 * s) (y + 8.5 * s)
	pendown t
	setheading t 0
	forward t $ 4.6 * s
	penup t
	goto t (x + 2.8 * s) (y + 10 * s)
	pendown t
	setheading t 0
	forward t $ 5.4 * s
	penup t
	goto t (x + 5.5 * s) (y + 8.5 * s)
	pendown t
	setheading t (- 90)
	forward t $ 2 * s
	right t 40
	forward t $ 3.5 * s
	penup t
	backward t $ 3.5 * s
	left t 80
	pendown t
	forward t $ 3.5 * s
	penup t

suu :: Turtle -> Double -> IO ()
suu t s = do
	pencolor t "red"
	(x, y) <- position t
	setheading t (- 45)
	goto t x (y + s)
	pendown t
	forward t $ 1.5 * s
	penup t
	goto t (x + 5 * s) (y + s)
	right t 90
	pendown t
	forward t $ 1.5 * s
	penup t
	goto t x (y + 3.5 * s)
	pendown t
	setheading t 0
	forward t $ 5 * s
	penup t
	goto t (x + 2.5 * s) (y + s)
	right t 90
	pendown t
	forward t $ 6 * s
	penup t
	goto t (x + 1.5 * s) (y + 4.8 * s)
	pendown t
	right t 38
	forward t $ 2.3 * s
	penup t
	goto t (x + 3.5 * s) (y + 4.8 * s)
	pendown t
	left t 76
	forward t $ 2.3 * s
	penup t
	pencolor t "black"
	goto t (x + 1.8 * s) (y + 8 * s)
	pendown t
	setheading t (- 110)
	forward t $ 4 * s
	left t 80
	forward t $ 3 * s
	penup t
	goto t (x + 4 * s) (y + 9 * s)
	pendown t
	setheading t (- 115)
	forward t $ 5 * s
	penup t
	goto t (x + 0 * s) (y + 9 * s)
	pendown t
	setheading t 0
	forward t $ 5 * s
	penup t
	goto t (x + 7.2 * s) (y + 0.5 * s)
	pendown t
	setheading t (- 109)
	forward t $ 5 * s
	penup t
	goto t (x + 6.8 * s) (y + 2.3 * s)
	pendown t
	setheading t 0
	forward t $ 4 * s
	penup t
	goto t (x + 9.5 * s) (y + 2.3 * s)
	pendown t
	setheading t (- 100)
	forward t $ 6 * s
	right t 20
	forward t $ 6 * s
	penup t
	goto t (x + 6.4 * s) (y + 2.6 * s)
	pendown t
	setheading t (- 85)
	forward t $ 6 * s
	left t 25
	forward t $ 5 * s
	penup t

gata :: Turtle -> Double -> IO ()
gata t s = do
	penup t
	(x, y) <- position t
	setheading t 0
	goto t (x + 2 * s) (y + 1.0 * s)
	pendown t
	forward t $ 4.2 * s
	penup t
	goto t (x + 1 * s) (y + 4.2 * s)
	pendown t
	forward t $ 5.2 * s
	penup t
	goto t (x + 3 * s) (y + 1.5 * s)
	pendown t
	setheading t (- 95)
	forward t $ 4 * s
	right t 20
	forward t $ 4 * s
	penup t
	goto t (x + 5 * s) (y + 1.5 * s)
	pendown t
	setheading t (- 90)
	forward t $ 6 * s
	penup t
	goto t (x + 7.5 * s) (y + 1.0 * s)
	pendown t
	setheading t (- 90)
	forward t $ 5 * s
	penup t
	goto t (x + 10 * s) (y + 0.7 * s)
	pendown t
	forward t $ 8 * s
	penup t
	goto t (x + 3 * s) (y + 10 * s)
	pendown t
	setheading t 0
	forward t $ 5 * s
	penup t
	goto t (x + 6 * s) (y + 8 * s)
	setheading t (- 90)
	pendown t
	forward t $ 5 * s
	penup t
	goto t (x + s) (y + 13.5 * s)
	pendown t
	setheading t 0
	forward t $ 10 * s
	penup t

gen :: Turtle -> Double -> IO ()
gen t s = do
	(x, y) <- position t
	penup t
	goto t (x + 5 * s) (y + 0.5 * s)
	setheading t (- 90)
	pendown t
	forward t $ 2 * s
	penup t
	goto t (x + 0 * s) (y + 2.5 * s)
	setheading t 0
	pendown t
	forward t $ 10 * s
	penup t
	goto t (x + 3 * s) (y + 4.5 * s)
	pendown t
	setheading t 0
	forward t $ 4 * s
	penup t
	goto t (x + 3 * s) (y + 7 * s)
	pendown t
	setheading t 0
	forward t $ 4 * s
	penup t
	goto t (x + 2 * s) (y + 9.5 * s)
	pendown t
	setheading t 0
	replicateM_ 2 $
		forward t (6 * s) >> right t 90 >> forward t (4 * s) >> right t 90
	penup t

go :: Turtle -> Double -> IO ()
go t s = do
	(x, y) <- position t
	setheading t (- 90)
	goto t (x + 3 * s) (y + 0.5 * s)
	pendown t
	forward t $ 2 * s
	penup t
	goto t x (y + 2.5 * s)
	pendown t
	setheading t 0
	forward t $ 5.2 * s
	penup t
	goto t (x + 1.2 * s) (y + 5 * s)
	pendown t
	setheading t 0
	forward t $ 3.6 * s
	penup t
	goto t (x + 1.2 * s) (y + 7.5 * s)
	pendown t
	setheading t 0
	forward t $ 3.6 * s
	penup t
	goto t (x + 0.6 * s) (y + 10 * s)
	pendown t
	setheading t 0
	replicateM_ 2 $
		forward t (4 * s) >> right t 90 >> forward t (3.5 * s) >> right t 90
	penup t
	goto t (x + 7 * s) (y + 1.5 * s)
	pendown t
	setheading t 0
	forward t $ 5 * s
	penup t
	goto t (x + 9 * s) (y + 1.5 * s)
	pendown t
	setheading t (- 110)
	forward t $ 3 * s
	left t 10
	forward t $ 3 * s
	penup t
	goto t (x + 7 * s) (y + 4 * s)
	pendown t
	setheading t 0
	forward t $ 4 * s
	right t 90
	forward t $ 4 * s
	penup t
	goto t (x + 6.5 * s) (y + 8 * s)
	pendown t
	setheading t 0
	forward t $ 6 * s
	penup t
	goto t (x + 6.5 * s) (y + 9.5 * s)
	pendown t
	setheading t 0
	replicateM_ 2 $
		forward t (5 * s) >> right t 90 >> forward t (4 * s) >> right t 90
