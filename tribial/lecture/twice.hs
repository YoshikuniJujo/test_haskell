data Twice a = Twice a a deriving Show

mapTwice :: (a -> b) -> Twice a -> Twice b
mapTwice f (Twice x y) = Twice (f x) (f y)
