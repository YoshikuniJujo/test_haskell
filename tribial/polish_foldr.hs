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
