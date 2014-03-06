import Graphics.X11.Turtle
import Text.XML.YJSVG hiding (topleft)
import Control.Monad

r :: Double
r = 1

bttm, lft :: Double
bttm = 160 * r
lft = 30 * r

main :: IO ()
main = do
	f <- openField
	topleft f
	onkeypress f $ return . (/= 'q')
	t <- newTurtle f
	flushoff t
	penup t

	goto t lft (bttm - 20 * r)
	setheading t 0
	pendown t
	forward t (250 * r)
	penup t

	goto t (lft + 50 * r) bttm
	setheading t 90
	pendown t
	forward t (150 * r)
	penup t

	goto t (lft + 50 * r) (bttm - 20 * r)
	setheading t 30
	pendown t
	forward t (200 * r)
	penup t

	backward t (120 * r)
	left t 90
	forward t (20 * r)
	write t "Monospace" 10 "dist"
	backward t (20 * r)
	right t 90

	backward t (60 * r)
	pendown t
	right t 90
	replicateM_ 30 $ right t 1 >> forward t (0.35 * r)
	penup t

	setheading t 30
	forward t (15 * r)
	write t "Monospace" 10 "rad"

	hideturtle t
--	waitField f

	getSVG t >>= putStr . showSVG (350 * r) (180 * r)
