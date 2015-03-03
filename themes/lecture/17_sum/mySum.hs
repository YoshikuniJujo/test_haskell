mySum :: [Integer] -> Integer
mySum [] = 0
mySum (x : xs) = x + mySum xs
