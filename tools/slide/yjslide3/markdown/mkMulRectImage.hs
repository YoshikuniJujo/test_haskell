import Graphics.X11.Turtle hiding (left)
import Text.XML.YJSVG hiding (topleft)
import Control.Monad

r :: Double
r = 1

bottom :: Double
bottom = 180 * r

left :: Double
left = 30 * r

main :: IO ()
main = do
	f <- openField
	topleft f
	onkeypress f $ return . (/= 'q')
	t <- newTurtle f
	penup t
	flushoff t

	goto t left (bottom - 20 * r)
	setheading t 0
	pendown t
	forward t (250 * r)
	penup t
	goto t (left + 50 * r) bottom
	setheading t 90
	pendown t
	forward t (170 * r)
	penup t
	goto t (left + 50 * r) (bottom - 20 * r)

	line t
	memori t

	caption t "(x, y)"

	replicateM 2 $ do
		line t
		memori t

	caption t "(x', y')"

	hideturtle t
--	waitField f

	getSVG t >>= putStr . showSVG (390 * r) (190 * r)

caption :: Turtle -> String -> IO ()
caption t txt = do
	setheading t 0
	forward t (10 * r)
	setheading t (- 90)
	forward t (10 * r)
	write t "Monospace" (10 * r) txt
	backward t (10 * r)
	setheading t 0
	backward t (10 * r)

line :: Turtle -> IO ()
line t = do
	setheading t 30
	pendown t
	forward t (70 * r)
	penup t
	

memori :: Turtle -> IO ()
memori t = do
	setheading t 120
	backward t (5 * r)
	pendown t
	forward t (10 * r)
	penup t
	backward t (5 * r)
