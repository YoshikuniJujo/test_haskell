import Control.Monad
import Graphics.X11.Turtle

import Template

ratio :: Double
ratio = 10

main :: IO ()
main = run (30, 190) 3 (330, 210) $ \t -> do
	(x0, y0) <- position t

	pendown t
	forward t 280
	penup t
	goto t x0 y0
	pendown t
	setheading t 90
	forward t 180
	penup t

	goto t x0 y0
	setheading t 30
	pendown t
	forward t $ 10 * ratio
	pencolor t "gray"
	forward t $ 10 * ratio
	pencolor t "black"
	forward t 3
	pencolor t "gray"
	forward t $ 10 * ratio - 3
	pencolor t "black"
	forward t 2
	penup t
	
	pencolor t "black"
	goto t (x0 + 5 * ratio) (y0 - 1 * ratio)
	write t "KochiGothic" 16 "pi/6"

	goto t (x0 + 4 * ratio) y0
	setheading t 90
	pendown t
	replicateM_ 6 $ forward t (ratio * 1 / 3) >> left t (180 / 6 / 6)
