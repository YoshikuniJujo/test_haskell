import Control.Applicative
import Data.Maybe
import Data.List
import Data.Function

data Matrix = Matrix [[Double]] deriving Show

sample :: Matrix
sample = Matrix [
	[1, 2, 1, 0, 0, 800],
	[3, 4, 0, 1, 0, 1800],
	[3, 1, 0, 0, 1, 1500],
	[-20, -30, 0, 0, 0, 0] ]

sample2 :: Matrix
sample2 = Matrix [
	[1, 1, 0, 0, 1],
	[1, 0, 1, 0, 1],
	[0, 1, 0, 1, 1],
	[0, 0, 1, 1, 1],
	[-9, -10, -2, -4, 0]
	]

pivot :: Matrix -> (Int, Int)
pivot (Matrix m) = (i, j)
	where
	l = last m
	j = minimumBy (compare `on` (l !!)) [0 .. length l - 1]
--	i = fromJust $ find
--		((&&) <$> all (>= 0) . (m !!) <*> (> 0) . (!! j) . (m !!))
--		[0 .. length m - 2]
	i = minimumBy (compare `on` (\k -> last (m !! k) / m !! k !! j))
		[0 .. length m - 2]

eliminate :: Matrix -> Int -> Int -> Matrix
eliminate (Matrix m) i j =
	Matrix $ map (eliminate1 j h) u ++ [h] ++ map (eliminate1 j h) l
	where
	(u, h : l) = splitAt i m

eliminate1 :: Int -> [Double] -> [Double] -> [Double]
eliminate1 j r2 r1 = zipWith (-) r1 $ map (* (r1 !! j / r2 !! j)) r2

step :: Matrix -> Matrix
step m = uncurry (eliminate m) $ pivot m
