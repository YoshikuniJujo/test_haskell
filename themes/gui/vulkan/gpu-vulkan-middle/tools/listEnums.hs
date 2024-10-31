{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.List

main :: IO ()
main = interact $ unlines
	. intercalate [""] . map sort
	. classify ["FlagBits", "KHR", "EXT", "INTEL", "NV", "AMD"]
--	. (\(x, y) -> sort x ++ [""] ++ sort y)
--	. partition ("FlagBits" `isSuffixOf`)
	. map ((!! 2) . words)
	. filter ("typedef enum" `isPrefixOf`)
	. lines

classify :: [String] -> [String] -> [[String]]
classify [] xs = [xs]
classify (sfx : sfxs) xs = ys : classify sfxs zs
	where (ys, zs) = partition (sfx `isSuffixOf`) xs
