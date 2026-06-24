{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Data.Time
import System.Environment

main :: IO ()
main = do
	[bs, ex] <- getArgs
	putStrLn . formatTime defaultTimeLocale (fmt bs ex) =<< getZonedTime

fmt :: String -> String -> String
fmt bs ex = bs ++ "_%0Y%0m%0d_%H%M%S_%Z." ++ ex
