module Fibonacci where

fib 0 = (0, 0)
fib 1 = (0, 1)
fib n = let
	(c1, f1) = fib $ n - 1
	(c2, f2) = fib $ n - 2 in
	(c1 + c2 + 1, f1 + f2)

fibC 0 = 0
fibC 1 = 0
fibC n = fibC (n - 1) + fibC (n - 2) + 1

fibC' (a, _) 0 = a
fibC' (a, b) n = fibC' (b, a + b + 1) (n - 1)
