import Control.Monad
import Graphics.X11.Turtle
import Text.XML.YJSVG hiding (topleft)

import qualified BBTree as B

main :: IO ()
main = do
	f <- openField
	topleft f
	onkeypress f (return . (/= 'q'))
	t <- newTurtle f
	penup t
	goto t 230 20
	pensize t 3

	tree t B.sample 180 55 2

	svg <- getSVG t
	putStrLn $ showSVG 480 250 svg

	waitField f

tree :: Show a =>
	Turtle -> B.Tree a (B.Tip a) -> Double -> Double -> Double -> IO ()
tree t bbt w h rt
	| B.null bbt = return ()
	| otherwise = case B.decompose bbt of
		Left (l, d, r) -> tree2 t l d r w h rt
		Right (l, c, o, f, r) -> tree3 t l c o f r w h rt

tree2 :: Show a => Turtle ->
	B.Tree a (B.Tip a) -> a -> B.Tree a (B.Tip a) ->
	Double -> Double -> Double -> IO ()
tree2 t l d r w h rt = do
	(x0, y0) <- position t
	goto t (x0 - 7) (y0 - 3)
	write t "KochiGothic" 15 $ show d

	when (not $ B.null l) $ do
		goto t (x0 - 10) (y0 + 4)
		pendown t
		goto t (x0 - w * 2 / 3) (y0 + h)
		penup t
		goto t (x0 - w * 2 / 3 - 3) (y0 + h + 20)
		tree t l (w / rt) h (rt * 3 / 2)

	when (not $ B.null r) $ do
		goto t (x0 + 10) (y0 + 4)
		pendown t
		goto t (x0 + w * 2 / 3) (y0 + h)
		penup t
		goto t (x0 + w * 2 / 3 + 3) (y0 + h + 20)
		tree t r (w / rt) h (rt * 3 / 2)

tree3 :: Show a => Turtle ->
	B.Tree a (B.Tip a) -> a -> B.Tree a (B.Tip a) -> a -> B.Tree a (B.Tip a) ->
	Double -> Double -> Double -> IO ()
tree3 t l c o f r w h rt = do
	(x0, y0) <- position t
	goto t (x0 - 15) (y0 - 3)
	write t "KochiGothic" 15 $ show c
	goto t (x0 + 3) (y0 - 3)
	write t "KochiGothic" 15 $ show f

	when (not $ B.null l) $ do
		goto t (x0 - 20) (y0 + 4)
		pendown t
		goto t (x0 - w) (y0 + h)
		penup t
		goto t (x0 - w - 3) (y0 + h + 20)
		tree t l (w / rt) h (rt * 3 / 2)

	when (not $ B.null o) $ do
		goto t x0 (y0 + 4)
		pendown t
		goto t x0 (y0 + h)
		penup t
		goto t x0 (y0 + h + 20)
		tree t o (w / rt) h (rt * 3 / 2)

	when (not $ B.null r) $ do
		goto t (x0 + 20) (y0 + 4)
		pendown t
		goto t (x0 + w) (y0 + h)
		penup t
		goto t (x0 + w + 3) (y0 + h + 20)
		tree t r (w / rt) h (rt * 3 / 2)
