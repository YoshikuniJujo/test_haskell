{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Bool
import Data.Maybe
import Text.Read

main :: IO ()
main = do
	putStrLn "半径を入力してください"
	(putStrLn `mapM_`) . circle . readRadius =<< getLine

circle :: Double -> [String]
circle r = [
	[ bool ' ' '.' $ internal r y x | x <- [0 .. 2 * r] ] |
	y <- [0 .. 2 * r] ]

internal :: Double -> Double -> Double -> Bool
internal r y x = (y - r) ^ (2 :: Int) + (x - r) ^ (2 :: Int) <= r ^ (2 :: Int)

readRadius :: String -> Double
readRadius = fromMaybe (error "No such radius") . readMaybe
