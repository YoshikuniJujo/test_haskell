data List a = Nil | a :~ List a deriving Show

mapL :: (a -> b) -> List a -> List b
mapL f (x :~ xs) = f x :~ mapL f xs
mapL _ _ = Nil
