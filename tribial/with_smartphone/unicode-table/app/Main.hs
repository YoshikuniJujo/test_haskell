{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Arrow
import Data.Char
import Numeric

main :: IO ()
main = putStrLn "Slozsoft"

tableLines :: [String]
tableLines = map (\(i, c) -> ($ "") $ showHex i . ((" " ++ [c]) ++)) table

table :: [(Int, Char)]
table = map (id &&& chr) [0x3041 ..]

sepBy :: (a -> b -> Maybe a) -> a -> a -> [b] -> [[b]]
sepBy f i s (x : xs) = case f s x of
	Just s' -> let ys : yss = sepBy f i s' ys in (x : ys) : yss
	Nothing -> [x] : sepBy f i i xs
sepBy _ _ _ _ = []
