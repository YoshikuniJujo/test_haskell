main = do
	print (fib 40)
--	print (fib 35)
fib n = if n < 2 then 1 else fib (n - 1) + fib (n - 2)
fib' = fibIter 1 1
fibIter a b 0 = a
fibIter a b n = fibIter b (a + b) (n - 1)
