import Graphics.X11.Turtle

import Template
import BBTree

main :: IO ()
main = run (20, 70) 5 (540, 210) $ \t -> do
	(x0, y0) <- position t
	pencolor t "red"
	tree t 5
	pencolor t "black"
	goto t (x0 + 80) y0
	tree t 5
	goto t (x0 + 160) y0
	tree t 5
	goto t (x0 + 65) (y0 - 20)
	pencolor t "green"
	value t 5
	goto t (x0 + 145) (y0 - 20)
	pencolor t "black"
	value t 5
	goto t (x0 + 230) y0
	arrow t 5
	goto t (x0 + 315) (y0 + 40)
	pencolor t "red"
	value t 5
	goto t (x0 + 250) (y0 + 60)
	tree t 5
	goto t (x0 + 330) (y0 + 60)
	tree t 5
	pencolor t "black"
	goto t (x0 + 380) y0
	tree t 5
	goto t (x0 + 460) y0
	tree t 5
	pencolor t "green"
	goto t (x0 + 365) (y0 - 20)
	value t 5
	goto t (x0 + 445) (y0 - 20)
	pencolor t "black"
	value t 5
	goto t (x0 + 20) (y0 + 135)
	write t "KochiGothic" 18 "(2) 値がふたつのノードの左が高くなる"
