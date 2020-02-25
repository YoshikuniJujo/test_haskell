{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

calcConwayChained :: [Integer] -> Integer
calcConwayChained = f . reverse

f [] = 1
f [p] = p
f [q, p] = p ^ q
f (1 : ns) = f ns
f (_ : 1 : ns) = f ns
f (q : p : ns) = f $ q - 1 : f (q : p - 1 : ns) : ns
