{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Ch15AllTheCommonPrefixesV1 where

import Data.Bool (bool)
-- import Data.List (tails)

type Result = [Int]
type State = (Result, (Int, Int, Int))

allcp :: Eq a => [a] -> Result
-- allcp xs = llcp xs <$> init (tails xs)
allcp xs = fst $ until (done n) (step xs) ([n], (0, 0, 1)) where n = length xs

done :: Int -> State -> Bool
done n (_as, (_i, _p, k)) = k == n

step :: Eq a => [a] -> State -> State
step xs (as, (i, p, k))
	| k >= i + p = (snoc as a, (k, a, k + 1))
	| q /= r = (snoc as (min q r), (i, p, k + 1))
	| q == r = (snoc as b, (k, b, k + 1))
	| otherwise = error "never occur"
	where
	q = as !! (k - i)
	r = p - (k - i)
	a = llcp xs $ drop k xs
	b = q + llcp (drop q xs) (drop (q + k) xs)

llcp :: Eq a => [a] -> [a] -> Int
llcp [] _ = 0; llcp _ [] = 0
llcp (x : xs) (y : ys) = bool 0 (1 + llcp xs ys) (x == y)

snoc :: [a] -> a -> [a]
snoc xs x = xs ++ [x]
