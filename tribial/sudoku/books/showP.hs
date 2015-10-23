import Control.Arrow
import Data.List
import Data.Function

type Cell = (Int, Int)

type Board = [(Cell, Int)]

main :: IO ()
main = putStr $ format problem

format :: Board -> String
format b = unlines . flip map [0 .. 8] $ \y ->
	(\l -> map (maybe '.' (head . show) . (`lookup` l)) [0 .. 8])
		$ map (first fst) $ filter ((== y) . snd . fst) b

problem :: Board
problem = [
	((3, 0), 8), ((5, 0), 1),
	((6, 1), 4), ((7, 1), 3),
	((0, 2), 5),
	((4, 3), 7), ((6, 3), 8),
	((6, 4), 1),
	((1, 5), 2), ((4, 5), 3),
	((0, 6), 6), ((7, 6), 7), ((8, 6), 5),
	((2, 7), 3), ((3, 7), 4),
	((3, 8), 2), ((6, 8), 6) ]
