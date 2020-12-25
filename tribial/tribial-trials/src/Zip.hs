{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Zip where

zip' :: [a] -> [b] -> [(a, b)]
zip' = zipWith (,)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f xs ys = uncurry f <$> zip xs ys
