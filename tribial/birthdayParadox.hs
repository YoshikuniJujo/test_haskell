{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Numeric

foo :: Int -> Int -> Rational
foo i n = 1 - product (fromIntegral <$> [2 ^ i - (fromIntegral n :: Integer) + 1 .. 2 ^ i]) / (2 ^ i) ^ n

bar :: Int -> Int -> String
bar i n = showFFloat (Just 3) (fromRational $ foo i n * 100) . ('%' :) $ ""

baz :: Int -> Int -> IO ()
baz i n = putStrLn $ bar i n

foo' :: Integer -> Integer -> Rational
foo' (fromInteger -> i) n = 1 - product [i - fromInteger n + 1 .. i] / i ^ n

foo'' :: Integer -> Integer -> Rational
foo'' i n = foo' (2 ^ i) n

baz' :: Integer -> Integer -> IO ()
baz' i n = putStrLn $ showFFloat Nothing (fromRational $ foo'' i n * 100) . ('%' :) $ ""
