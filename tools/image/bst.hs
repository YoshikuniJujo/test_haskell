import Control.Applicative
import Control.Monad
import Graphics.X11.Turtle
import Text.XML.YJSVG hiding (topleft)
import System.Environment

import qualified BST

main :: IO ()
main = do
	ns <- map read . lines <$> (readFile . head =<< getArgs)

	f <- openField
	topleft f
	onkeypress f (return . (/= 'q'))
	t <- newTurtle f
	penup t
	goto t 180 20
	pensize t 3

	tree t (mkBST ns :: BST.BST Int) 80 45

	svg <- getSVG t
	putStrLn $ showSVG 340 220 svg
	waitField f

tree :: Show a => Turtle -> BST.BST a -> Double -> Double -> IO ()
tree t bst w h
	| BST.null bst = return ()
	| otherwise = do
		penup t
		(x0, y0) <- position t
		goto t (x0 - 3) (y0 - 3)
		write t "KochiGothic" 15 . show $ BST.value bst

		when (not . BST.null $ BST.left bst) $ do
			goto t (x0 - 6) (y0 + 4)
			pendown t
			goto t (x0 - w) (y0 + h)
			penup t
			goto t (x0 - w - 5) (y0 + h + 20)
			tree t (BST.left bst) (w / 2) h
			penup t

		when (not . BST.null $ BST.right bst) $ do
			goto t (x0 + 6) (y0 + 4)
			pendown t
			goto t (x0 + w) (y0 + h)
			penup t
			goto t (x0 + w + 3) (y0 + h + 20)
			tree t (BST.right bst) (w / 2) h


mkBST :: Ord a => [a] -> BST.BST a
mkBST = foldr BST.insert BST.empty . reverse

sample :: BST.BST Int
sample = mkBST [8, 3, 10, 1, 6, 14, 4, 7, 13]
