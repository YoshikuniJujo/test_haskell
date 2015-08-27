import Control.Applicative

example1, example2, example3, example4 :: [String]
example1 = ["+", "3", "4"]
example2 = ["*", "+", "1", "5", "+", "2", "3"]
example3 = ["/", "*", "+", "1", "5", "+", "2", "3", "4"]
example4 = ["/", "*", "+", "8", "5", "-", "2", "5", "4"]

operators :: [(String, Integer -> Integer -> Integer)]
operators = [("+", (+)), ("-", (-)), ("*", (*)), ("/", div)]

polish :: [String] -> Maybe [Integer]
polish [] = Just []
polish (s : ss) = case lookup s operators of
	Just o -> case polish ss of
		Just (x : y : ns) -> Just $ x `o` y : ns
		_ -> Nothing
	_ -> case reads s of
		(n, "") : _ -> (n :) <$> polish ss
		_ -> Nothing
