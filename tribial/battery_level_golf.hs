{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Control.Concurrent
import Data.List
import System.Directory
import System.FilePath

main :: IO ()
main = do
	run1
	threadDelay 10000000
	main

path :: FilePath
path = "/sys/class/power_supply"

getPath :: IO FilePath
getPath = (path </>) . head . filter isBat <$> getDirectoryContents path
	where
	isBat = ("BAT" `isPrefixOf`)

run1 :: IO ()
run1 = do
	fp <- getPath
	n <- read <$> readFile (fp </> "charge_now")
	f <- read <$> readFile (fp </> "charge_full")
	putStrLn $ '[' : graph 74 (n / f) ++ "] " ++
		show (round $ n / f * 100 :: Int) ++ "%"

graph :: Int -> Double -> String
graph n d =
	a `replicate` '*' ++ (n - a) `replicate` ' '
	where a = round (fromIntegral n * d)
