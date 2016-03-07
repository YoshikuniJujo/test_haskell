data Boo a = Boo Int a deriving Show

foo, bar :: (a -> b) -> Boo a -> Boo b
foo f (Boo n x) = Boo (n * 10) (f x)
bar f (Boo n x) = Boo (n + 3) (f x)
