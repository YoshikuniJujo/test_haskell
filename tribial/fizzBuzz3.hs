import Data.List

converts n m = cycle $ const m : replicate (n - 1) id

c = map (foldr1 (.)) $ transpose $
	zipWith converts [15, 5, 3] ["Fizz Buzz", "Buzz", "Fizz"]

main = mapM_ putStrLn $ zipWith ($) (tail c) $ map show [1 .. 20]
