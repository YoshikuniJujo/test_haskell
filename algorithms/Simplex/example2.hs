import Data.List
import Data.Function

data Matrix = Matrix [[Double]] deriving Show

sample :: Matrix
sample = Matrix [
	[1, 1, 0, 0, 1],
	[1, 0, 1, 0, 1],
	[0, 1, 0, 1, 1],
	[0, 0, 1, 1, 1],
	[-9, -10, -2, -4, 0] ]

pivotj :: Matrix -> Int
pivotj (Matrix m) = minimumBy (compare `on` (l !!)) [0 .. length l - 1]
	where l = last m

pivoti :: Matrix -> Int -> Int
pivoti (Matrix m) j = minimumBy (compare `on` \k -> last (m !! k) / m !! k !! j)
	$ filter ((> 0) . (\k -> m !! k !! j)) [0 .. length m - 2]

pivot :: Matrix -> (Int, Int)
pivot m = let j = pivotj m; i = pivoti m j in (i, j)

eliminate :: Matrix -> Int -> Int -> Matrix
eliminate (Matrix m) i j =
	Matrix $ map (eliminate1 j h) u ++ [h] ++ map (eliminate1 j h) l
	where
	(u, h : l) = splitAt i m

eliminate1 :: Int -> [Double] -> [Double] -> [Double]
eliminate1 j r2 r1 = zipWith (-) r1 $ map (* (r1 !! j / r2 !! j)) r2

step :: Matrix -> Matrix
step m = uncurry (eliminate m) $ pivot m
