example1, example2, example3, example4 :: [String]
example1 = ["+", "3", "4"]
example2 = ["*", "+", "1", "5", "+", "2", "3"]
example3 = ["/", "*", "+", "1", "5", "+", "2", "3", "4"]
example4 = ["/", "*", "+", "8", "5", "-", "2", "5", "4"]

operators :: [(String, Integer -> Integer -> Integer)]
operators = [("+", (+)), ("-", (-)), ("*", (*)), ("/", div)]

polish :: [String] -> [Integer] -> [Integer]
polish [] ns = ns
polish (s : ss) ns = case lookup s operators of
	Just o -> case polish ss ns of
		x : y : ns -> x `o` y : ns
		_ -> error "polish: operator needs two numbers"
	_ -> read s : polish ss ns
