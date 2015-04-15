nary :: Integer -> Integer -> Integer -> Integer
nary n a b = a * n + b

octal :: Integer -> Integer -> Integer
octal = nary 8

decimal :: Integer -> Integer -> Integer
decimal = nary 10
