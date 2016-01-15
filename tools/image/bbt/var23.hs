import Graphics.X11.Turtle

import Template
import BBTree

main :: IO ()
main = run (20, 110) 5 (430, 180) $ \t -> do
	(x0, y0) <- position t

	goto t (x0 + 20) (y0 - 85)
	write t "KochiGothic" 28 "2-3"

	pencolor t "gray"

	goto t (x0 + 5) y0
	labeledTree t "k" 4
	goto t (x0 + 65) y0
	labeledTree t "l'" 4
	goto t (x0 + 125) y0
	labeledTree t "m" 4

	goto t (x0 + 55) (y0 - 10)
	labeledValue t "a" 4

	goto t (x0 + 115) (y0 - 10)
	labeledValue t "b" 4

	goto t (x0 - 10) (y0 - 60)
	labeledLong t "l" 4

	goto t (x0 + 195) (y0 - 70)
	labeledValue t "d" 4

	goto t (x0 + 220) y0
	labeledTree t "q" 4
	goto t (x0 + 280) y0
	labeledTree t "r'" 4
	goto t (x0 + 340) y0
	labeledTree t "s" 4
	goto t (x0 + 270) (y0 - 10)
	labeledValue t "g" 4
	goto t (x0 + 330) (y0 - 10)
	labeledValue t "h" 4
	goto t (x0 + 206) (y0 - 60)
	labeledLong t "r" 4
