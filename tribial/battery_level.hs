{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Concurrent
import Data.List
import System.Directory
import System.FilePath

isBat :: FilePath -> Bool
isBat = ("BAT" `isPrefixOf`)

path :: FilePath
path = "/sys/class/power_supply"

getPath :: IO FilePath
getPath = (path </>) . head . filter isBat <$> getDirectoryContents path

main :: IO ()
main = do
	run1
	threadDelay 10000000
	main

isNow :: FilePath -> Bool
isNow = (||) <$> (== "charge_now") <*> (== "energy_now")

isFull :: FilePath -> Bool
isFull = (||) <$> (== "charge_full") <*> (== "energy_full")

getNow :: IO FilePath
getNow = do
	p <- getPath
	(p </>) . head . filter isNow <$> getDirectoryContents p

getFull :: IO FilePath
getFull = do
	p <- getPath
	(p </>) . head . filter isFull <$> getDirectoryContents p

run1 :: IO ()
run1 = do
	now <- read <$> (readFile =<< getNow)
	full <- read <$> (readFile =<< getFull)
	putStr $ '[' : showGraph 74 (now / full) ++ "] "
	putStrLn $ show (round $ now / full * 100 :: Int) ++ "%"

showGraph :: Int -> Double -> String
showGraph n d =
	a `replicate` '*' ++ (n - a) `replicate` ' '
	where a = round (fromIntegral n * d)
