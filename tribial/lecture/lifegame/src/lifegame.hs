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

type Board = [Row]
type Row = [Bool]

showBoard :: Board -> [String]
showBoard = map . map $ bool '-' '*'

readBoard :: [String] -> Board
readBoard = map $ map (== '*')

game :: Board -> [Board]
game = iterate $ next . count . neighbors

type Count = [[(Bool, Int)]]
type Neighbors = [(Bool, [Bool])]

next :: Count -> Board
next = map . map $ \(h, n) -> bool (n == 3) (n == 2 || n == 3) h

count :: [Neighbors] -> Count
count = map . map . second $ sum . map fromEnum

neighbors :: Board -> [Neighbors]
neighbors = map (uncurry3 nbs) . triples (repeat False) . map (False :)

nbs :: Row -> Row -> Row -> Neighbors
nbs (tl : ts@(t : tr : _)) (l : hs@(h : r : _)) (bl : bs@(b : br : _)) =
	(h, [tl, t, tr, l, r, bl, b, br]) : nbs ts hs bs
nbs (tl : t : _) (l : h : _) (bl : b : _) = [(h, [tl, t, l, bl, b])]
nbs _ _ _ = []

triples :: a -> [a] -> [(a, a, a)]
triples d xs = tpl $ d : xs
	where
	tpl (p : ys@(c : f : _)) = (p, c, f) : tpl ys
	tpl [p, c] = [(p, c, d)]
	tpl _ = []

second :: (b -> c) -> (a, b) -> (a, c)
second f (x, y) = (x, f y)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z
