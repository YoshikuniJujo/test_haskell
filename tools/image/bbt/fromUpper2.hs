import Graphics.X11.Turtle

import Template
import BBTree

main :: IO ()
main = run (20, 90) 5 (300, 165) $ \t -> do
	(x0, y0) <- position t
	pencolor t "red"
	tree t 5
	pencolor t "black"
	goto t (x0 + 80) y0
	tree t 5
	pencolor t "green"
	goto t (x0 + 65) (y0 - 20)
	value t 5
	pencolor t "black"
	goto t (x0 + 160) y0
	tree t 5
	goto t (x0 + 145) (y0 - 20)
	value t 5
	goto t (x0 - 15) (y0 - 80)
	long t 15
