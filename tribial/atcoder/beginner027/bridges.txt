import Control.Arrow

main :: IO ()
main = interact $ (++ "\n") . show
	. (\(n, xs) -> let s = sum xs in if s `mod` n /= 0 then -1 else
		bridges (s `div` n) 0 0 xs)
	. (read *** map read . words) . (\[n, xs] -> (n, xs)) . lines

bridges :: Int -> Int -> Int -> [Int] -> Int
bridges _ _ _ [] = 0
bridges n i s (x : xs)
	| s' == n * i' = bridges n 0 0 xs
	| otherwise = 1 + bridges n i' s' xs
	where i' = i + 1; s' = s + x
