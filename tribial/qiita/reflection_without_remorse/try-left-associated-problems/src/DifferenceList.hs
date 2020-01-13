{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module DifferenceList where

type DiffList a = [a] -> [a]

toDiffList :: [a] -> DiffList a
toDiffList = (++.)

fromDiffList :: DiffList a -> [a]
fromDiffList = ($ [])

hello :: DiffList Char
hello = toDiffList "hello"

helloL, helloR :: DiffList Char
helloL = foldl (.) id $ replicate 5000 hello
helloR = foldr (.) id $ replicate 5000 hello

helloLString, helloRString :: String
helloLString = {-# SCC "LeftAssociatedHellos" #-} fromDiffList helloL
helloRString = {-# SCC "LeftAssociatedHellos" #-} fromDiffList helloR

(++.) :: [a] -> [a] -> [a]
[] ++. ys = ys
(x : xs) ++. ys = x : (xs ++. ys)
