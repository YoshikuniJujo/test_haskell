type Cont r a = (a -> r) -> r

ret :: a -> Cont r a
ret a = \k -> k a

bind :: Cont r a -> (a -> Cont r b) -> Cont r b
c `bind` f = \k -> c (\a -> f a k)

add x y = \k -> k $ x + y
mul x y = \k -> k $ x * y
sub x y = \k -> k $ x - y

h :: Int -> (String -> c) -> c
h n = \k -> k $ show n

g :: String -> ([String] -> e) -> e
g str = \k -> k $ replicate 3 str

f :: [String] -> (Int -> g) -> g
f strs = \k -> k $ length $ concat strs

callCC :: ((a -> Cont r b) -> Cont r a) -> Cont r a
callCC f = \k -> f (\a -> \_ -> k a) k
