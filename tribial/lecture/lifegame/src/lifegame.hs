{-# LANGUAGE TupleSections #-}

import Prelude hiding (readFile)
import System.IO.Strict (readFile)

import Control.Concurrent (threadDelay)
import Data.Foldable (for_)
import Data.Bool (bool)
import System.Environment (getArgs)

main :: IO ()
main = do
	args <- getArgs
	case args of
		fp : n : _ -> do
			b0 <- readBoard . lines <$> readFile fp
			for_ (read n `take` game b0) $ \b -> do
				putStrLn . unlines $ showBoard b
				threadDelay 100000
		_ -> putStrLn "Usage: lifegame foo.txt 100"

type Board = [[Bool]]

showBoard :: Board -> [String]
showBoard = map . map $ bool '-' '*'

readBoard :: [String] -> Board
readBoard = map $ map (== '*')

game :: Board -> [Board]
game = iterate $ next . count . neighbors False

type Count = [[(Bool, Int)]]
type Neighbors = [[(Bool, [Bool])]]

next :: Count -> Board
next = map . map $ \(h, n) -> bool (n == 3) (n == 2 || n == 3) h

count :: Neighbors -> Count
-- count = map . map . second $ sum . map fromEnum
count = map . map . second $ length . filter id

neighbors :: a -> [[a]] -> [[(a, [a])]]
neighbors d = map (uncurry3 nbs) . triples (d, repeat d) . map (d ,)

nbs :: (a, [a]) -> (a, [a]) -> (a, [a]) -> [(a, [a])]
nbs (tl, t : ts@(tr : _)) (l, h : hs@(r : _)) (bl, b : bs@(br : _)) =
	(h, [tl, t, tr, l, r, bl, b, br]) : nbs (t, ts) (h, hs) (b, bs)
nbs (tl, t : _) (l, h : _) (bl, b : _) = [(h, [tl, t, l, bl, b])]
nbs _ _ _ = []

triples :: a -> [a] -> [(a, a, a)]
triples d = tpl d
	where
	tpl p (c : fs@(f : _)) = (p, c, f) : tpl c fs
	tpl p [c] = [(p, c, d)]
	tpl _ _ = []

second :: (b -> c) -> (a, b) -> (a, c)
second f (x, y) = (x, f y)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z
