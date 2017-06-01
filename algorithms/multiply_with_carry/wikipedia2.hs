import Control.Arrow

rs :: [Int]
rs = zipWith ((+) . (7 *)) xs cs

xs :: [Int]
xs = 1 : map (`mod` 10) rs

cs :: [Int]
cs = 3 : map (`div` 10) rs
