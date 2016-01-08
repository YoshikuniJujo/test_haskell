import Graphics.X11.Turtle
import Text.XML.YJSVG hiding (topleft)

main :: IO ()
main = do
	f <- openField
	onkeypress f (return . (/= 'q'))
	t <- newTurtle f
	topleft f
	penup t
	goto t 120 20
	pensize t 3
	tree t (Node 8 (Node 4 (Leaf 10) (Leaf 2)) (Leaf 5))  60 45
	svg <- getSVG t
	putStrLn $ showSVG 200 160 svg
	waitField f

data Tree = Node Int Tree Tree | Leaf Int

tree :: Turtle -> Tree -> Double -> Double -> IO ()
tree t (Node c l r) w h = do
	penup t
 	(x0, y0) <- position t
	goto t (x0 - 3) y0
	write t "KochiGothic" 12 $ show c
	goto t (x0 - 6) (y0 + 4)
	pendown t
	goto t (x0 - w) (y0 + h)
	penup t
	goto t (x0 - w - 5) (y0 + h + 20)
	tree t l (w / 2) h
	penup t
	goto t (x0 + 6) (y0 + 4)
	pendown t
	goto t (x0 + w) (y0 + h)
	penup t
	goto t (x0 + w + 3) (y0 + h + 20)
	tree t r (w / 2) h
tree t (Leaf c) _ _ = do
	write t "KochiGothic" 12 $ show c
