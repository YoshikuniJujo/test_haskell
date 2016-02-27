import Data.List

add1 :: Bool -> Bool -> Bool -> (Bool, Bool)
add1 False b c = (b && c, b /= c)
add1 _ b c = (b || c, b == c)

add :: Bool -> [Bool] -> [Bool] -> [Bool]
add False bs [] = bs
add False [] cs = cs
add _ bs [] = add False bs [True]
add _ [] cs = add False [True] cs
add a (b : bs) (c : cs) = let (a', b') = add1 a b c in b' : add a' bs cs

(.+.) :: [Bool] -> [Bool] -> [Bool]
(.+.) = add False

fromI :: Integer -> [Bool]
fromI 0 = []
fromI n = odd n : fromI (n `div` 2)

toI :: [Bool] -> Integer
toI [] = 0
toI (b : bs) = (if b then 1 else 0) + 2 * toI bs
