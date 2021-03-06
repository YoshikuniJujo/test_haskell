import Graphics.X11.Turtle

import Template
import BBTree

main :: IO ()
main = run (20, 110) 5 (280, 180) $ \t -> do
	(x0, y0) <- position t

	goto t (x0 + 20) (y0 - 85)
	write t "KochiGothic" 28 "2-2"

	pencolor t "gray"

	goto t x0 y0
	labeledTree t "k" 4
	goto t (x0 + 60) y0
	labeledTree t "m" 4

	goto t (x0 + 50) (y0 - 10)
	labeledValue t "a" 4

	goto t (x0 - 10) (y0 - 60)
	labeledBox t "l" 4

	goto t (x0 + 123) (y0 - 70)
	labeledValue t "d" 4

	goto t (x0 + 145) y0
	labeledTree t "q" 4
	goto t (x0 + 195) (y0 - 10)
	labeledValue t "g" 4
	goto t (x0 + 205) y0
	labeledTree t "s" 4
	goto t (x0 + 136) (y0 - 60)
	labeledBox t "r" 4
