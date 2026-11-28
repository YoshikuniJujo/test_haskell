{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Char
import System.Process
import System.Directory
import System.FilePath

main :: IO ()
main = do
	binPath <- chomp
		<$> readProcess "cabal" ["list-bin", "try-yj-javascript-value"] ""
	let	path = binPath <.> "jsexe" </> "all.js"
	createDirectoryIfMissing True "dist/bouncingBalls.jsexe"
	copyFile "index.html" "dist/index.html"
	copyFile path "dist/bouncingBalls.jsexe/all.js"

chomp :: String -> String
chomp = reverse . dropWhile isSpace . reverse
