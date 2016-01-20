import Graphics.X11.Turtle

import Template

ratio :: Double
ratio = 10

main :: IO ()
main = run (30, 190) 3 (330, 210) $ \t -> do
	(x0, y0) <- position t

	pendown t
	forward t 280
	penup t
	backward t 280
	left t 90
	pendown t
	forward t 180

	penup t
	goto t (x0 + 8 * ratio) y0
	setheading t 90
	pendown t
	forward t 10
	penup t
	backward t 30
	setheading t 0
	backward t 5
	write t "KochiGothic" 20 "8"

	goto t (x0 + 24 * ratio) y0
	setheading t 90
	pendown t
	forward t 10
	penup t
	backward t 30
	setheading t 0
	backward t 10
	write t "KochiGothic" 20 "24"

	goto t x0 (y0 - 5 * ratio)
	setheading t 0
	pendown t
	forward t 10
	penup t
	backward t 25
	setheading t 90
	backward t 7
	write t "KochiGothic" 20 "5"

	goto t x0 (y0 - 15 * ratio)
	setheading t 0
	pendown t
	forward t 10
	penup t
	backward t 35
	setheading t 90
	backward t 7
	write t "KochiGothic" 20 "15"

	goto t x0 y0
	pendown t
	goto t (x0 + 8 * ratio) (y0 - 5 * ratio)

	pencolor t "gray"
	goto t (x0 + 24 * ratio) (y0 - 15 * ratio)
	penup t

	pencolor t "black"
	setheading t 0
	backward t 2
	dot t 3

	goto t (x0 + 16 * ratio) (y0 - 10 * ratio)
	setheading t 90
	forward t 2
	dot t 3

	pencolor t "gray"
	pensize t 1
	goto t (x0 + 8 * ratio) y0
	pendown t
	goto t (x0 + 8 * ratio) (y0 - 5 * ratio)
	goto t x0 (y0 - 5 * ratio)

	penup t
	goto t (x0 + 24 * ratio) y0
	pendown t
	goto t (x0 + 24 * ratio) (y0 - 15 * ratio)
	goto t x0 (y0 - 15 * ratio)
