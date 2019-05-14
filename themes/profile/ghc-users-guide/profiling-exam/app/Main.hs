module Main where

main :: IO ()
main = print $ f 40 + g 40
	where
	f n = fib n
	g n = fib (n `div` 2)

fib :: Word -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
