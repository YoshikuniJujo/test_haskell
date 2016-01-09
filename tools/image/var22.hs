import Graphics.X11.Turtle

import Template
import BBTree

main :: IO ()
main = run (20, 130) 5 (460, 240) $ \t -> do
	(x0, y0) <- position t

	pencolor t "gray"

	goto t x0 y0
	tree t 4
	goto t (x0 + 13) (y0 + 28)
	label t "k"

	goto t (x0 + 60) y0
	tree t 4
	goto t (x0 + 73) (y0 + 28)
	label t "m"

	goto t (x0 + 50) (y0 - 10)
	value t 4
	goto t (x0 + 42) (y0 - 18)
	label t "a"

	goto t (x0 - 10) (y0 - 60)
	box t 12
	goto t (x0 - 5) (y0 - 30)
	label t "l"

	goto t (x0 + 160) (y0 - 90)
	value t 5

	goto t (x0 + 190) y0
	tree t 5
	goto t (x0 + 255) (y0 - 20)
	value t 5
	goto t (x0 + 270) y0
	tree t 5
	goto t (x0 + 180) (y0 - 80)
	box t 15
