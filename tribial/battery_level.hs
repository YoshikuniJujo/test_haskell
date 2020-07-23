{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Concurrent

main :: IO ()
main = do
	run1
	threadDelay 10000000
	main

run1 :: IO ()
run1 = do
	now <- read <$> readFile "/sys/class/power_supply/BAT0/charge_now"
	full <- read <$> readFile "/sys/class/power_supply/BAT0/charge_full"
	putStr $ '[' : showGraph 74 (now / full) ++ "] "
	putStrLn $ show (round $ now / full * 100 :: Int) ++ "%"

showGraph :: Int -> Double -> String
showGraph n d =
	a `replicate` '*' ++ (n - a) `replicate` ' '
	where a = round (fromIntegral n * d)
