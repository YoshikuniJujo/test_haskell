h :: Char; ello :: String
h : ello = "hello"

some :: [a] -> [a]
some xs@(x : _) = x : xs
some [] = []

fib@(1:tfib) = 1 : 1 : [ a + b | (a, b) <- zip fib tfib ]
