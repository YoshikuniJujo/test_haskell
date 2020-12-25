{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Zip where

import Control.Monad
import Control.Monad.Identity

zip' :: [a] -> [b] -> [(a, b)]
zip' = zipWith (,)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f xs ys = uncurry f <$> zip xs ys

zipWith'' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith'' f xs ys = runIdentity $ zipWithM (\x y -> Identity $ f x y) xs ys
