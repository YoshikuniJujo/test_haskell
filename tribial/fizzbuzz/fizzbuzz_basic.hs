main :: IO ()
main = putStrLn . unwords $ take 100 fizzbuzz

fizzbuzz :: [String]
fizzbuzz = do
	n <- [1 ..]
	return $ case (n `mod` 3, n `mod` 5) of
		(0, 0) -> "FizzBuzz"
		(0, _) -> "Fizz"
		(_, 0) -> "Buzz"
		_ -> show n
