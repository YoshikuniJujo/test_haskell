import Graphics.X11.Turtle
import Control.Monad

initTurtle :: IO Turtle
initTurtle = openField >>= newTurtle >>= \t ->
	speed t "slowest" >> shape t "turtle" >> shapesize t 2 2 >> return t

shikaku :: Turtle -> Double -> IO ()
shikaku t size = replicateM_ 4 (forward t size >> left t 90)

moji :: Turtle -> IO ()
moji t = getLine >>= write t "Kochi-Gothic" 10
