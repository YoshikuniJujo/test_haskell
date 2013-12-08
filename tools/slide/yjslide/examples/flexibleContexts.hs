{-# LANGUAGE FlexibleContexts #-}

import Data.Array.IArray

testArray :: Array Bool Int
testArray = array (False, True) [(False, 80), (True, 95)]

-- testArray2 :: j

some :: (IArray a Int, Ix i) => a i Int -> i -> Int
some xs i = 8 * xs ! i

some2 :: (IArray a (Maybe Int), Ix i) => a i (Maybe Int) -> i -> Maybe Int
some2 xs i = xs ! i

other :: Eq [a] => a -> [a] -> Bool
other x xs = [x] == xs

class PackContexts c

data EqOrdEnum a b c

instance (Eq a, Ord b, Enum c) => PackContexts (EqOrdEnum a b c)

-- mistery :: PackContexts (EqOrdEnum a b c) => a -> a -> b -> b -> c -> (Bool, Int)
mistery x1 x2 y1 y2 z = (x1 == x2 && y1 > y2, fromEnum z)
