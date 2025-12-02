{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import AtCoder.Adv20251201

main :: IO ()
main = do
	_ <- getLine
	ns <- (read <$>) . words <$> getLine
	putStrLn . unwords $ show <$> previous ns
