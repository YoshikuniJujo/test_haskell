import Control.Arrow

xn :: Int -> Int
xn n | n < 1 = 1
xn n = (7 * xn (n - 1) + cn (n - 1)) `mod` 10

cn :: Int -> Int
cn 0 = 3
cn n = (7 * xn (n - 1) + cn (n - 1)) `div` 10

nextX :: Int -> Int -> Int
nextX x c = (7 * x + c) `mod` 10

nextC :: Int -> Int -> Int
nextC x c = (7 * x + c) `div` 10

xcs :: [(Int, Int)]
xcs = zip xs cs

xs :: [Int]
xs = 1 : map (uncurry nextX) xcs

cs :: [Int]
cs = 3 : map (uncurry nextC) xcs
