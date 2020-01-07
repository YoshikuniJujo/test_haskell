{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module LeftAssociatedList where

hello :: String
hello = "hello"

helloL, helloR :: String
helloL = {-# SCC "LeftAssociatedHellos" #-}
	foldl (++.) "" $ replicate 5000 hello

helloR = {-# SCC "RightAssociatedHellos" #-}
	foldr (++.) "" $ replicate 5000 hello

(++.) :: [a] -> [a] -> [a]
[] ++. ys = ys
(x : xs) ++. ys = x : (xs ++. ys)
