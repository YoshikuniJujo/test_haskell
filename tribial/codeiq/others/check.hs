check :: Integer -> Integer -> Integer
check x y = x * y `div` gcd x y 

nums = [ (a, b) | a <- [1 ..], b <- [a ..], check a b == product [1 .. 50] ]
