module Template (run) where

import Control.Monad
import Graphics.X11.Turtle
import Text.XML.YJSVG hiding (topleft)

run :: (Double, Double) -> Double -> (Double, Double) -> (Turtle -> IO a) -> IO ()
run (x0, y0) s (w, h) act = do
	f <- openField
	topleft f
	onkeypress f (return . (/= 'q'))
	t <- newTurtle f
	penup t
	goto t x0 y0
	pensize t s

	act t

	svg <- getSVG t
	putStrLn $ showSVG w h svg

	waitField f
