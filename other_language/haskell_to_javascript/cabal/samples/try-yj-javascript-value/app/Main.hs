{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.Object qualified as JS.Object

main :: IO ()
main = do
	putStrLn "Hello, Haskell!"
	JS.Value.consoleLog (123 :: Int)
	JS.Value.consoleLog (3.14 :: Double)
	JS.Value.consoleLog True
	JS.Value.consoleLog ([1, 2, 3] :: [Int])
	o <- JS.Object.new
	JS.Object.set o "foo" "bar"
	JS.Object.set o "bar" (123 :: Int)
	JS.Value.consoleLog =<< JS.Object.freeze o
