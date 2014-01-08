module WriteTree (
	writeTree, BinTree(..), rotateL, rotateR, mapR
) where

import Graphics.X11.Turtle

data BinTree a = Empty | Bin a (BinTree a) (BinTree a) deriving Show

testMain :: IO ()
testMain = do
	f <- openField
	topleft f
	t <- newTurtle f
	penup t
	writeTree t show 10 10 230 100 testTree
	hideturtle t
	onkeypress f $ return . (/= 'q')
	waitField f

writeTree :: Turtle -> (a -> String) -> Double -> Double -> Double -> Double -> BinTree a -> IO ()
writeTree _ _ _ _ _ _ Empty = return ()
writeTree t sw sz dx x y (Bin v l r) = do
	goto t x (y - sz / 2)
	penup t
	goto t (x - sz / 2) (y + sz)
	write t "KochiGothic" sz (sw v)
	goto t (x - sz * dx / 10) (y + sz * 1.5)
	pendown t
	writeTree t sw sz (dx / 2) (x - dx * sz) (y + 4 * sz) l
	penup t
	goto t (x + sz * dx / 10) (y + sz * 1.5)
	pendown t
	writeTree t sw sz (dx / 2) (x + dx * sz) (y + 4 * sz) r
	penup t

testTree :: BinTree Int
testTree = Bin 8
	(Bin 15
		(Bin 99
			(Bin 3
				(Bin 888 Empty Empty)
				Empty)
			(Bin 1 Empty Empty))
		(Bin 250 Empty Empty))
	(Bin 18
		(Bin 851 Empty Empty)
		(Bin 223 Empty Empty))

rotateL :: BinTree a -> BinTree a
rotateL (Bin x lx (Bin y ly ry)) = Bin y (Bin x lx ly) ry
rotateL t = t

rotateR :: BinTree a -> BinTree a
rotateR (Bin x (Bin y ly ry) rx) = Bin y ly (Bin x ry rx)
rotateR t = t

mapR :: (BinTree a -> BinTree a) -> BinTree a -> BinTree a
mapR f (Bin x l r) = Bin x l (f r)
mapR _ t = t
