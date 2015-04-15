import Data.List

main :: IO ()
main = interact $ show . sum . nub . (\(n : ns) -> noneighbours True n $ sort ns)
	. map read . words

noneighbours :: Bool -> Int -> [Int] -> [Int]
noneighbours b p [] = if b then [p] else []
noneighbours b p (n : ns)
	| abs (p - n) == 1 = noneighbours False n ns
	| otherwise = (if b then (p :) else id) $ noneighbours True n ns
