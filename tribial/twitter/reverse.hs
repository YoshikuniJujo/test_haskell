{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Array

main :: IO ()
main = putStrLn "reverse"

reverse1 :: [a] -> [a]
reverse1 = rev [] where rev r = \case [] -> r; x : xs -> rev (x : r) xs

reverse2 :: Int -> [a] -> [a]
reverse2 n xs = (a !) <$> [n - 1, n - 2 .. 0] where a = listArray (0, n - 1) xs
