module ForProfile where

import System.Random

sample0_100 :: [Int]
sample0_100 = take 100 . randoms $ mkStdGen 8

sample0_10000 :: [Int]
sample0_10000 = take 10000 . randoms $ mkStdGen 8

sample0_100000 :: [Int]
sample0_100000 = take 100000 . randoms $ mkStdGen 8

sample1_100000 :: [Int]
sample1_100000 = take 100000 . randoms $ mkStdGen 9

sample2_100000 :: [Int]
sample2_100000 = take 100000 . randoms $ mkStdGen 10

sampleX_N :: Int -> [Int]
sampleX_N n = [1 .. 10 ^ 6] ++ take (n * 1000) (cycle [10 ^ 6 + 1, 10 ^ 6 + 2])

sampleNormal_N :: Int -> [Int]
sampleNormal_N n = take (10 ^ 6 + n * 1000) . randomRs (1, 10 ^ 6) $ mkStdGen 8
