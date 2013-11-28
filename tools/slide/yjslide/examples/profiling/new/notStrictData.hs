import Prelude hiding (sum)

data Sum = Sum Integer deriving Show

sum :: Sum -> [Integer] -> Sum
sum s [] = s
sum (Sum s) (x : xs) = sum (Sum $ s + x) xs

main = print $ sum (Sum 0) [0 .. 10000000]
