{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Shuffle2 where

import Data.List
import Data.Time

a, c, m :: Int
a = 1664525
c = 1013904223
m = 2 ^ 32

randoms :: Int -> [Int]
randoms = iterate (\x -> (a * x + c) `mod` m)

randomRs :: Int -> Int -> [Int]
randomRs r = (range r <$>) . randoms

range :: Int -> Int -> Int
range r x = x `div` (m `div` r)

time0 :: UTCTime
time0 = UTCTime (fromGregorian 1980 1 1) 0

randomRIO :: Int -> IO Int
randomRIO r = do
	s <- (`diffUTCTime` time0) <$> getCurrentTime
	pure $ randomRs r (fromIntegral . floor $ s * 100) !! 100

format :: String -> [Char] -> String
format "" _ = ""
format ('_' : s) (c : cs) = c : format s cs
format (c : s) cs = c : format s cs

shuffle :: [a] -> IO [a]
shuffle xs = do
	i <- randomRIO $ product [1 .. length xs]
	pure $ permutations xs !! i
