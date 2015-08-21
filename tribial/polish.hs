example1, example2, example3, example4 :: [String]
example1 = ["+", "3", "4"]
example2 = ["*", "+", "1", "5", "+", "2", "3"]
example3 = ["/", "*", "+", "1", "5", "+", "2", "3", "4"]
example4 = ["/", "*", "+", "8", "5", "-", "2", "5", "4"]

type State = [Integer]

update :: String -> State -> State
update "+" (x : y : ns) = x + y : ns
update "*" (x : y : ns) = x * y : ns
update n ns = read n : ns

operator :: [(String, Integer -> Integer -> Integer)]
operator = [("+", (+)), ("-", (-)), ("*", (*)), ("/", div)]

polish :: [String] -> [Integer] -> [Integer]
polish [] ns = ns
polish (s : ss) ns = case lookup s operator of
	Just o -> case polish ss ns of
		x : y : ns -> x `o` y : ns
		_ -> error "polish: operator need two numbers"
	_ -> read s : polish ss ns
