{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

pascal :: [[Int]]
pascal = (`iterate` [1]) $ (1 :) . (++ [1]) . (zipWith (+) <$> id  <*> tail)

showN :: Show a => Int -> a -> String
showN n x = replicate (m `div` 2) ' ' ++ s ++ replicate (m - m `div` 2) ' '
	where s = show x; m = n - length s

showPascalLine :: Int -> [Int] -> String
showPascalLine n = (showN n =<<)

maybeShow :: Show a => Int -> Either Int a -> String
maybeShow n (Left m) = replicate (n * m `div` 2) ' '
maybeShow n (Right x) = showN n x

padPascal :: Int -> [[Either Int Int]]
padPascal n = zipWith (:) ((Left . (n -)) <$> [1 ..]) ((Right <$>) <$> pascal)

example1, example80 :: IO ()
example1 = putStrLn `mapM_` take 22 ((maybeShow 7 =<<) <$> padPascal 22)
example80 = putStrLn `mapM_` take 16 ((maybeShow 5 =<<) <$> padPascal 16)
