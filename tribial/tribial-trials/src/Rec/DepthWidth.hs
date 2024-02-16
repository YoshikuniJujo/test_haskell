{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Rec.DepthWidth where

nums :: Int -> [Int]
nums n = n : nums (2 * n) ++ nums (2 * n + 1)

step :: Int -> [Int]
step n = [2 * n, 2 * n + 1]

nums' :: Int -> [Int]
nums' n = n : (nums' =<< step n)

numsW :: [Int] -> [Int]
numsW (n : ns) = n : numsW (ns ++ step n)
