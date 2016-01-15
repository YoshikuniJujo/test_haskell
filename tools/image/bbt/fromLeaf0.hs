import Graphics.X11.Turtle

import Template
import BBTree

main :: IO ()
main = run (20, 140) 5 (460, 240) $ \t -> do
	(x0, y0) <- position t

	pencolor t "red"
	goto t (x0 - 10) (y0 - 80)
	box t 15

	pencolor t "green"
	goto t (x0 + 170) (y0 - 90)
	value t 5

	pencolor t "black"
	goto t (x0 + 200) (y0 - 80)
	box t 15
