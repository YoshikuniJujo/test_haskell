import Control.Monad
import Text.XML.YJSVG
import Graphics.X11.Turtle

patterns :: [(Double, Int, String)]
patterns = [
	(125, 1, "slowest"),
	(60, 3, "slow"),
	(- 5, 4, "normal"),
	(- 70, 5, "fast"),
	(- 135, 6, "fastest")
	]

main :: IO ()
main = do
	f <- openField
	onkeypress f $ return . (/= 'q')
	t <- newTurtle f
--	speed t "slowest"
	speed t "fast"
	shape t "turtle"
--	shapesize t 2 2
	pencolor t "darkgreen"
	penup t
	forM_ patterns $ \(y, n, s) -> do
		goto t (- 195) (y - 20)
		write t "Kochi-Gothic" 12 $ "n = " ++ show n
		goto t (- 200) y
		speed t s
		pendown t
		koch t 200 n
		penup t
	hideturtle t
--	svg <- getSVG t
--	putStr $ showSVG 410 380 svg
	waitField f

koch :: Turtle -> Double -> Int -> IO ()
koch t x n | n < 1 = forward t x
koch t x n = do
	koch t (x / 3) (n - 1) >> left t 60 
	koch t (x / 3) (n - 1) >> right t 120
	koch t (x / 3) (n - 1) >> left t 60
	koch t (x / 3) (n - 1)
