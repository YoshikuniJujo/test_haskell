import Control.Arrow
import Data.List
import Data.Function

main :: IO ()
main = interact $ unlines . uncurry draw . second process . trace 0 (0, 0)
	. map operation . words

data Op = U | D | L | R deriving (Show, Read)

operation :: String -> (Op, Int)
operation (o : n) = (read [o], read n)

trace :: Int -> (Int, Int) -> [(Op, Int)] -> (Int, [(Int, Int)])
trace mx pos [] = (mx + 1, [pos])
trace mx pos ((op, 0) : ops) = trace mx pos ops
trace mx pos ((op, n) : ops) = let
	npos@(_, nx) = move op pos in
	(pos :) `second` trace (max mx nx) (move op pos) ((op, n - 1) : ops)

move :: Op -> (Int, Int) -> (Int, Int)
move U (y, x) = (y - 1, x)
move D (y, x) = (y + 1, x)
move L (y, x) = (y, x - 1)
move R (y, x) = (y, x + 1)

process :: [(Int, Int)] -> [[Int]]
process = map (map snd) . groupBy (on (==) fst) . nub . sort

draw :: Int -> [[Int]] -> [String]
draw mx = map (drawLine mx)

drawLine :: Int -> [Int] -> String
drawLine mx [] = replicate mx '_'
drawLine mx (0 : xs) = '#' : drawLine (mx - 1) (map (subtract 1) xs)
drawLine mx xs = '_' : drawLine (mx - 1) (map (subtract 1) xs)
