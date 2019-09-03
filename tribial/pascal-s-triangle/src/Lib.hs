module Lib where

pascal :: [[Int]]
pascal = (`iterate` [1]) $ (1 :) . (++ [1]) . (zipWith (+) <$> id  <*> tail)
