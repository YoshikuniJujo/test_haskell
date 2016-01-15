import Graphics.X11.Turtle

import Template
import BBTree

main :: IO ()
main = run (20, 70) 5 (380, 210) $ \t -> do
	(x0, y0) <- position t
	pencolor t "red"
	tree t 5
	pencolor t "black"
	goto t (x0 + 80) y0
	tree t 5
	goto t (x0 + 65) (y0 - 20)
	value t 5
	goto t (x0 + 150) y0
	arrow t 5
	goto t (x0 + 235) (y0 + 40)
	pencolor t "red"
	value t 5
	goto t (x0 + 170) (y0 + 60)
	tree t 5
	goto t (x0 + 250) (y0 + 60)
	tree t 5
	pencolor t "black"
	goto t (x0 + 300) y0
	tree t 5
	goto t (x0 + 285) (y0 - 20)
	value t 5
	goto t (x0 + 20) (y0 + 135)
	write t "KochiGothic" 18 "(1) 値がひとつのノードの左が高くなる"
