import Prelude hiding (sum)

sum :: Integer -> [Integer] -> Integer
sum s [] = s
-- sum s (n : ns) = sum (s + n) ns
-- sum s (n : ns) = flip sum ns $! (s + n)
sum s (n : ns) = let r = s + n in r `seq` sum r ns

main :: IO ()
main = print $ sum 0 [0 .. 10000000]
