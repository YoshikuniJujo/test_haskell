{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Arrow
import Data.Char
import System.Environment
import Numeric

main :: IO ()
main = do
	n_ : _ <- getArgs
	let	n = read n_
	mapM_ (putStrLn . unwords) . sepWords n $ take 30 tableLines

tableLines :: [String]
tableLines = map (\(i, c) -> ($ "") $ showHex i . ((" " ++ [c]) ++)) table

table :: [(Int, Char)]
table = map (id &&& chr) [0x3041 ..]

sepWords :: Int -> [String] -> [[String]]
sepWords n = sepBy (\c s -> if c + length s > n then Nothing else Just $ c + length s + 1) 0 0

sepBy :: (a -> b -> Maybe a) -> a -> a -> [b] -> [[b]]
sepBy f i s xa@(x : xs) = case f s x of
	Just s' -> let ys : yss = sepBy f i s' xs in (x : ys) : yss
	Nothing -> [] : sepBy f i i xa
sepBy _ _ _ _ = [[]]
