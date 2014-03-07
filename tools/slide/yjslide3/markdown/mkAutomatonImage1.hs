import AutomatonLib
import Graphics.X11.Turtle
import Text.XML.YJSVG hiding (topleft)

r :: Double
r = 2

main :: IO ()
main = do
	f <- openField
	topleft f
	onkeypress f $ return . (/= 'q')
	t <- newTurtle f
	flushoff t
	penup t
	pensize t r

	initialQ t r (20 * r) (40 * r) "q1"
	selfQ t r "0"
	nextQ t r "1" "q2"
	selfQ t r "1"
	acceptQ t r
	nextQ t r "0" "q3"
	backQ t r "0, 1"

--	waitField f

	getSVG t >>= putStrLn . showSVG (300 * r) (100 * r)
