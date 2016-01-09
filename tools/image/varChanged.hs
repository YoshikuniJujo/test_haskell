import Graphics.X11.Turtle

import Template
import BBTree

main :: IO ()
main = run (20, 20) 5 (450, 70) $ \t -> do
	(x0, y0) <- position t

	pencolor t "gray"

	goto t x0 y0
	labeledTree t "l" 4
	setheading t 0
	goto t (x0 + 48) (y0 + 18)
	arrow t 3
	goto t (x0 + 80) y0
	labeledTree t "l'" 4

	goto t (x0 + 150) y0
	labeledTree t "o" 4
	setheading t 0
	goto t (x0 + 198) (y0 + 18)
	arrow t 3
	goto t (x0 + 230) y0
	labeledTree t "o'" 4

	goto t (x0 + 300) y0
	labeledTree t "r" 4
	setheading t 0
	goto t (x0 + 348) (y0 + 18)
	arrow t 3
	goto t (x0 + 380) y0
	labeledTree t "r'" 4
