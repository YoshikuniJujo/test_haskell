import Prelude hiding (sum)

sum :: Integer -> [Integer] -> Integer
sum s [] = s
sum s (n : ns) = sum (s + n) ns

main :: IO ()
main = print $ sum 0 [0 .. 10000000]
