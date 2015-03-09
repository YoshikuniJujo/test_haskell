myProduct :: [Integer] -> Integer
myProduct [] = 1
myProduct (x : xs) = x * myProduct xs
