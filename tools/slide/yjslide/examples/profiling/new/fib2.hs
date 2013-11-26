fib :: (Int, Int) -> Int -> Int
fib (a, _) 0 = a
fib (a, b) n = fib (b, a + b) (n - 1)

main :: IO ()
main = print $ fib (0, 1) 40
