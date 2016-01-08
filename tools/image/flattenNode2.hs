import Graphics.X11.Turtle

import Template
import BBTree

main :: IO ()
main = run (20, 70) 5 (350, 150) $ \t -> do
	(x0, y0) <- position t
	pencolor t "red"
	tree t 5
	goto t (x0 + 80) y0
	tree t 5
	goto t (x0 + 65) (y0 - 20)
	value t 5
	pencolor t "black"
	goto t (x0 + 160) y0
	tree t 5
	goto t (x0 + 240) y0
	tree t 5
	pencolor t "green"
	goto t (x0 + 145) (y0 - 20)
	value t 5
	pencolor t "black"
	goto t (x0 + 225) (y0 - 20)
	value t 5
	goto t (x0 + 20) (y0 + 75)
	write t "KochiGothic" 18 "(2) 木の高さをそろえる"
