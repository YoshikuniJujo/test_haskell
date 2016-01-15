import Graphics.X11.Turtle

import Template
import BBTree

main :: IO ()
main = run (20, 70) 5 (540, 210) $ \t -> do
	(x0, y0) <- position t
	pencolor t "black"
	emptyTree t 5
	pencolor t "red"
	goto t (x0 + 80) y0
	emptyTree t 5
	pencolor t "black"
	goto t (x0 + 160) y0
	emptyTree t 5
	goto t (x0 + 65) (y0 - 20)
	pencolor t "green"
	value t 5
	goto t (x0 + 145) (y0 - 20)
	pencolor t "black"
	value t 5
	goto t (x0 + 230) y0
	arrow t 5

	goto t (x0 + 405) (y0 + 40)
	pencolor t "red"
	value t 5
	goto t (x0 + 340) (y0 + 60)
	emptyTree t 5
	goto t (x0 + 420) (y0 + 60)
	emptyTree t 5

	pencolor t "black"
	goto t (x0 + 300) y0
	emptyTree t 5
	goto t (x0 + 460) y0
	emptyTree t 5
	pencolor t "green"
	goto t (x0 + 365) (y0 - 20)
	value t 5
	goto t (x0 + 445) (y0 - 20)
	pencolor t "black"
	value t 5
	goto t (x0 + 20) (y0 + 135)
	write t "KochiGothic" 18 "(3) 値がふたつの末端の中央への追加"
