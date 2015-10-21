import Control.Applicative
import Data.List

queens :: Int -> Int -> [Int] -> [Int] -> [Int] -> [[Int]]
queens _ 0 _ _ _ = [[]]
queens n i ls hs rs = qs >>= \q ->
	map (q :) $ queens n (i - 1)
		(map (subtract 1) $ q : ls)
		(q : hs)
		(map (+ 1) $ q : rs)
	where
	qs = [0 .. n - 1] \\ (ls ++ hs ++ rs)
