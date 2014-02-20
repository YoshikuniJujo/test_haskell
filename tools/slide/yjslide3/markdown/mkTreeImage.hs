import Graphics.X11.Turtle
import Text.XML.YJSVG hiding (topleft)

ratio :: Double
ratio = 2

main :: IO ()
main = do
	f <- openField
	topleft f
	onkeypress f $ return . (/= 'q')
	t <- newTurtle f
	penup t
	goto t (120 * ratio) (20 * ratio)
	drawTree t ratio 50 pathTree
	waitField f
	svg <- getSVG t
--	putStrLn $ showSVG (145 * ratio) (120 * ratio) svg
	putStrLn $ showSVG (250 * ratio) (120 * ratio) svg

data Tree = Node Char Tree Tree | Leaf Char deriving Show

pathTree :: Tree
pathTree = Node 'a'
	(Node 'b'
		(Leaf 'd')
		(Node 'e' (Leaf 'f') (Leaf 'g')))
	(Leaf 'c')

drawTree :: Turtle -> Double -> Double -> Tree -> IO ()
drawTree t r _ (Leaf c) = do
	(x, y) <- position t
	goto t (x - r * 3) (y - r)
	write t "Monospace" (r * 10) [c]
drawTree t r w (Node c t1 t2) = do
	(x, y) <- position t
	goto t (x - r * 3) (y - r)
	write t "Monospace" (r * 10) [c]
	goto t (x - r * 3) (y + r * 1 / 2)
	pendown t
	goto t (x - r * (w - 2)) (y + r * (30 - 10))
	penup t
	goto t (x - r * w) (y + r * 30)
	drawTree t r (w / 2) t1
	goto t (x + r * 3) (y + r * 1 / 2)
	pendown t
	goto t (x + r * (w - 2)) (y + r * (30 - 10))
	penup t
	goto t (x + r * w) (y + r * 30)
	drawTree t r (w / 2) t2
