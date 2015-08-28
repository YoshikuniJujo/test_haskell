import Data.List
import Data.Array

exist :: Int -> Int -> [Int] -> Bool
exist 0 0 _ = True
exist 0 _ _ = False
exist t m ks = any (\k -> exist (t - 1) (m - k) ks) ks

exist4 :: Int -> [Int] -> Bool
exist4 m ks =
	or [ m == ka + kb + kc + kd | ka <- ks, kb <- ks, kc <- ks, kd <- ks ]

exist4_2 :: Int -> [Int] -> Int -> Bool
exist4_2 n ks m =
	or [ binSearch 0 (n - 1) (m - ka - kb - kc) a |
		ka <- ks, kb <- ks, kc <- ks ]
	where
	a = listArray (0, n - 1) $ sort ks

binSearch :: (Ix i, Integral i, Ord a) => i -> i -> a -> Array i a -> Bool
binSearch mn mx x0 a
	| x == x0 = True
	| mn >= m = False
	| x < x0 = binSearch (m + 1) mx x0 a
	| otherwise = binSearch mn (m - 1) x0 a
	where
	m = (mn + mx) `div` 2
	x = a ! m
