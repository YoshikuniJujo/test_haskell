import Control.Monad
import Graphics.X11.Turtle

import Template
import BBTree

main :: IO ()
main = run (20, 70) 5 (310, 130) $ \t -> do
	(x0, y0) <- position t
	emptyTree t 5
	goto t (x0 + 80) y0
	setheading t 0
	arrow t 5
	goto t (x0 + 150) y0
	emptyTree t 5
	goto t (x0 + 230) y0
	emptyTree t 5
	setheading t 0
	goto t (x0 + 215) (y0 - 20)
	pendown t
	value t 5
	hideturtle t
