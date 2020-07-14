foo [] = []
foo (x : xs) = x : foo xs

bar s [] = s
bar s (x : xs) = bar (x : s) xs

foo' s [] = s []
foo' s (x : xs) = foo' (s . (x :)) xs

bar' [] = id
bar' (x : xs) = bar' xs . (x :)
