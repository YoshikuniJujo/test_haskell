import Text.XML.YJSVG hiding (topleft)
import Graphics.X11.Turtle

main :: IO ()
main = do
	f <- openField
	topleft f
	onkeypress f $ return . (/= 'q')
	t <- newTurtle f
	penup t
	pensize t 2
	goto t 145 0
	showBinTree (: "") t 80 sampleTree
	svg <- getSVG t
	writeFile "binaryTree.svg" $ showSVG 230 250 svg
	waitField f

data BinTree a = Bin a (BinTree a) (BinTree a) | Leaf a
	deriving Show

sampleTree :: BinTree Char
sampleTree = Bin 'a'
	(Bin 'b'
		(Leaf 'd')
		(Bin 'e' (Leaf 'f') (Leaf 'g')))
	(Leaf 'c')

showBinTree :: (a -> String) -> Turtle -> Double -> BinTree a -> IO ()
showBinTree sh t _ (Leaf x) = do
	(x, y) <- position t
	goto t (x - fromIntegral (length s) * 4) (y + 18)
	write t "KochiGothic" 15 s
	where
	s = sh x
showBinTree sh t w (Bin x l r) = do
	(x, y) <- position t
	goto t (x - fromIntegral (length s) * 4) (y + 18)
	write t "kochiGothic" 15 s
	penup t
	goto t (x - 5) (y + 25)
	pendown t
	goto t (x - w) (y + 70)
	penup t
	goto t (x - w - 3) (y + 70)
	showBinTree sh t (w / 2) l
	goto t (x + 5) (y + 25)
	pendown t
	goto t (x + w) (y + 70)
	penup t
	goto t (x + w + 3) (y + 70)
	showBinTree sh t (w * 2 / 3) r
	where
	s = sh x
