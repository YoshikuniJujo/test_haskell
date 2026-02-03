{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Tools where

import Data.Char
import Numeric

strToHexStr :: String -> String
strToHexStr = concat . (sh <$>) . map ord
	where
	sh n = let s = showHex n "" in replicate (2 - length s) '0' ++ s

separate :: Int -> String -> [String]
separate _ "" = []
separate n s = take n s : separate n (drop n s)
