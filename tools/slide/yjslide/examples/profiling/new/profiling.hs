fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fib40 = {-# SCC "fib_40" #-} fib 40

fib20 = fib 20

main :: IO ()
main = print $ {-# SCC "fib40" #-} fib 40
