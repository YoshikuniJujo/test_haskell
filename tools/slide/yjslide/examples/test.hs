h :: Char; ello :: String
h : ello = "hello"

some :: [a] -> [a]
some xs@(x : _) = x : xs
some [] = []

fib@(1:tfib) = 1 : 1 : [ a + b | (a, b) <- zip fib tfib ]

-- before
bfr = (\x y -> \z -> y ((\u -> u) z) x) 25

-- weak head normal form
whnf = \y -> (\z -> y ((\u -> u) z)) 25

-- head normal form
hnf = \y -> y ((\u -> u) 25)

-- normal form
nf = \y -> y 25
