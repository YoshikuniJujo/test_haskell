{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Concurrent

main :: IO ()
main = do
	putStrLn "foo"
	threadDelay 3000000
	putStrLn "bar"
