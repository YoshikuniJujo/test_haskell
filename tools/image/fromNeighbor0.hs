import Graphics.X11.Turtle

import Template
import BBTree

main :: IO ()
main = run (20, 140) 5 (460, 240) $ \t -> do
	(x0, y0) <- position t

	pencolor t "red"
	tree t 5
	goto t (x0 + 80) y0
	tree t 5
	goto t (x0 + 65) (y0 - 20)
	value t 5
	pencolor t "black"
	goto t (x0 - 10) (y0 - 80)
	box t 15

	pencolor t "green"
	goto t (x0 + 160) (y0 - 90)
	value t 5

	pencolor t "blue"
	goto t (x0 + 190) y0
	tree t 5
	goto t (x0 + 255) (y0 - 20)
	value t 5

	pencolor t "black"
	goto t (x0 + 270) y0
	tree t 5
	goto t (x0 + 350) y0
	tree t 5
	goto t (x0 + 335) (y0 - 20)
	value t 5
	goto t (x0 + 175) (y0 - 80)
	long t 15
