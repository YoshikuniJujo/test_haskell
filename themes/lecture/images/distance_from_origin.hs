import Text.XML.YJSVG hiding (topleft)
import Graphics.X11.Turtle

main :: IO ()
main = do
	f <- openField
	topleft f
	onkeypress f $ return . (/= 'q')
	t <- newTurtle f
	penup t

	goto t 40 5
	pendown t
	setheading t (- 90)
	forward t 130
	left t 90
	forward t 130
	penup t
	goto t 30 17
	write t "KochiGothic" 12 "y"
	goto t 160 145
	write t "KochiGothic" 12 "x"

	pencolor t "red"
	goto t 41.5 133.5
	pendown t
	goto t 120 80
	penup t
	pencolor t "black"

	goto t 40 80
	pendown t
	pencolor t "gray"
	forward t 80
	penup t
	setheading t 0
	pencolor t "black"
	backward t 2
	right t 90
	backward t 2
	dot t 4
	setheading t 0
	forward t 2
	right t 90
	forward t 2
	pencolor t "gray"
	pendown t
	forward t 55

	hideturtle t
	svg <- getSVG t
	writeFile "distance_from_origin.svg" $ showSVG 170 145 svg
	waitField f
