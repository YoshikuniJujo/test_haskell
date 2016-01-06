import Control.Applicative
import Control.Monad
import Data.Function
import Data.Char
import Graphics.X11.Turtle
import Text.XML.YJSVG hiding (topleft)
import System.Environment

import qualified BST

main :: IO ()
main = do
	ns <- lines <$> (readFile . head =<< getArgs)

	f <- openField
	topleft f
	onkeypress f (return . (/= 'q'))
	t <- newTurtle f
	penup t
--	goto t 140 20
	goto t 90 20
	pensize t 3

	tree t (mkBST ns) 80 45

	svg <- getSVG t
	putStrLn $ showSVG 230 150 svg
	waitField f

tree :: Turtle -> BST.BST String -> Double -> Double -> IO ()
tree t bst w h
	| BST.null bst = return ()
	| otherwise = do
		penup t
		(x0, y0) <- position t
		goto t (x0 - 3) (y0 - 3)
		write t "KochiGothic" 15 $ BST.value bst

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


mkBST :: [String] -> BST.BST String
mkBST = foldr (BST.insertBy (compare `on` map toLower)) BST.empty . reverse
