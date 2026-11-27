module Main (main) where

import GHC.JS.Value

main :: IO ()
main = do
	putStrLn "Hello, Haskell!"
	consoleLog (123 :: Int)
	consoleLog (3.14 :: Double)
	consoleLog True
	consoleLog ([1, 2, 3] :: [Int])
