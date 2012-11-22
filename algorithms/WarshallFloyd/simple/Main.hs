module Main where

import WarshallFloyd

import System.Environment
import Control.Applicative

main :: IO ()
main = do
	fn : _ <- getArgs
	dists <- makeDists 1 5 . map (map read . words) . lines <$> readFile fn
	putStrLn $ show2D 4 dists
	putStrLn $ show2D 4 $ route dists
