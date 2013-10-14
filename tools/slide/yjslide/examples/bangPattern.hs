{-# LANGUAGE BangPatterns #-}

module Main where

some (!(x : xs)) = Just (x, xs)
some [] = Nothing

other !x = True

eight = let (x : xs) = [] in 8
eight' = let !(x : xs) = [] in 8

consMap :: (a -> b) -> b -> [a] -> [b]
consMap f y0 (x : xs) = y0 : consMap f (f x) xs
consMap _ y0 [] = y0 : []

ints = consMap (+ 1) 0 ints

expo2 = consMap (* 2) 1 expo2
