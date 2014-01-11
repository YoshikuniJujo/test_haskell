module WriteTree (
	writeTree, BinTree(..), rotateL, rotateR, mapR
) where

import Graphics.X11.Turtle

data BinTree a = Empty | Bin a (BinTree a) (BinTree a) deriving Show

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

rotateL :: BinTree a -> BinTree a
rotateL (Bin x lx (Bin y ly ry)) = Bin y (Bin x lx ly) ry
rotateL t = t

rotateR :: BinTree a -> BinTree a
rotateR (Bin x (Bin y ly ry) rx) = Bin y ly (Bin x ry rx)
rotateR t = t

mapR :: (BinTree a -> BinTree a) -> BinTree a -> BinTree a
mapR f (Bin x l r) = Bin x l (f r)
mapR _ t = t
