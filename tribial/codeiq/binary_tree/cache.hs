import Control.Arrow

main :: IO ()
main = interact $ (++ "\n") . show
	. (\n -> if even n then 0 else trees $ n `div` 2) . read

cases :: [Integer]
cases = map trees [0 ..]

trees :: Int -> Integer
trees 0 = 1
trees n = sum
	[ (cases !! l) * (cases !! r) | l <- [0 .. n - 1], let r = n - l - 1 ]

cache :: ([a] -> Int -> a) -> Int -> a
cache f = g where g = f rs; rs = map g [0 ..]

draw :: [Integer] -> Int -> Integer
draw _ 0 = 1
draw rs n = sum [ (rs !! l) * (rs !! r) | l <- [0 .. n - 1], let r = n - l - 1 ]

fib :: Num n => [n] -> Int -> n
fib _ n | n < 2 = fromIntegral n
fib fs n = fs !! (n - 1) + fs !! (n - 2)
