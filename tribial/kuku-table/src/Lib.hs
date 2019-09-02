{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

kuku :: [[Int]]
kuku = (<$> [1 .. 9]) <$> ((*) <$> [1 .. 9])

showN :: Show a => Int -> a -> String
showN n x = replicate (n - length s) ' ' ++ s where s = show x

kukuLines :: [String]
kukuLines = unwords . (showN 2 <$>) <$> kuku

header :: [String]
header = h : [replicate (length h) '-']
	where h = unwords $ (showN 2 <$>) [1 .. 9]

lefter :: [String]
lefter = "   |" : "---+" : ((++ " |") . showN 2 <$> [1 .. 9])

kukuTable :: [String]
kukuTable = zipWith (++) lefter (header ++ kukuLines)

putKukuTable :: IO ()
putKukuTable = putStrLn `mapM_` kukuTable
