import Graphics.X11.Turtle hiding (center)
import Text.XML.YJSVG hiding (topleft, center)

r :: Double
r = 2

top :: Double
top = 15 * r

center :: Double
center = 50 * r

main :: IO ()
main = do
	f <- openField
	topleft f
	onkeypress f $ return . (/= 'q')
	t <- newTurtle f
	flushoff t
	penup t
	goto t center top
	putTree t (30 * r) (30 * r) exam
--	waitField f
	getSVG t >>= putStrLn . showSVG (230 * r) (130 * r)

data BinTree a = Node (BinTree a) (BinTree a) | Leaf a deriving Show

exam :: BinTree String
exam = Node
	(Node (Leaf "a") (Node (Leaf "b") (Leaf "c")))
	(Leaf "d")

putTree :: Turtle -> Double -> Double -> BinTree String -> IO ()
putTree t _ _ (Leaf txt) = do
	penup t
	(x, y) <- position t
	goto t (x - 5 * r) (y + 12 * r)
	write t "Monospace" (10 * r) txt
	goto t x y
putTree t h w (Node l r) = do
	(x, y) <- position t
	pendown t
	goto t (x - w) (y + h)
	putTree t h (w / 2) l
	penup t
	goto t x y
	pendown t
	goto t (x + w) (y + h)
	putTree t h (w / 2) r
	penup t
