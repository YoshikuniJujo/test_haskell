import Data.List

mul, add :: Int -> Int -> Int
m `mul` n = (m * n) `mod` 1000
m `add` n = (m + n) `mod` 1000

facts :: [Int]
facts = map (foldl1' mul . enumFromTo 1) [1 ..]

main :: IO ()
main = print . foldl1' add $ take 14 facts
