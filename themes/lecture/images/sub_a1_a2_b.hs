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
	goto t 30 10
	sub t "red" "A1"
	goto t 130 10
	sub t (0x3f, 0x3f, 0xff) "B"
	goto t 230 10
	sub t "red" "A2"
	goto t 140 130
	state t
	penup t
	pensize t 3
	pencolor t "red"
	goto t 80 70
	pendown t
	setheading t (- 45)
	forward t 75
	arrowHead t
	goto t 90 115
	write t "KochiGothic" 18 "a"
	pencolor t (0x3f, 0x3f, 0xff)
	penup t
	goto t 170 70
	pendown t
	setheading t (- 90)
	forward t 50
	arrowHead t
	goto t 150 100
	write t "KochiGothic" 18 "b"
	goto t 205 125
	pencolor t "red"
	pendown t
	setheading t 45
	forward t 75
	arrowHead t
	pencolor t (0x3f, 0x3f, 0xff)
	goto t 250 110
	write t "KochiGothic" 18 "b"
	goto t 150 159
	pencolor t "red"
	write t "KochiGothic" 25 "a"
	pencolor t "black"
	goto t 164 159
	write t "KochiGothic" 25 "/"
	pencolor t (0x3f, 0x3f, 0xff)
	goto t 178 159
	write t "KochiGothic" 25 "b"
	hideturtle t
	svg <- getSVG t
	writeFile "sub_a1_a2_b.svg" $ showSVG 350 180 svg
	waitField f

arrowHead :: Turtle -> IO ()
arrowHead t = do
	penup t
	left t 30
	backward t 10
	pendown t
	forward t 10
	right t 60
	backward t 10
	penup t

sub :: ColorClass c => Turtle -> c -> String -> IO ()
sub t c n = do
	(x, y) <- position t
	setheading t 0
	pencolor t c
	pendown t
	beginfill t
	replicateM_ 2 $ forward t 80 >> right t 90 >> forward t 50 >> right t 90
	penup t
	endfill t
	pencolor t "black"
	goto t (x + 37 - fromIntegral (length n) * 6) (y + 35)
	write t "KochiGothic" 25 n

state :: Turtle -> IO ()
state t = do
	(x, y) <- position t
	setheading t 0
	pendown t
	pencolor t "yellow"
	beginfill t
	replicateM_ 2 $ forward t 60 >> right t 90 >> forward t 40 >> right t 90
	endfill t
