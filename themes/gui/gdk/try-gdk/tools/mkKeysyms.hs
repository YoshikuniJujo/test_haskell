{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.List
import System.Process

dstFile :: FilePath
dstFile = "./data/keysyms.txt"

main :: IO ()
main = do
	fp <- (++ "/gdk/gdkkeysyms.h") . drop 2 . head
		. filter ("gtk-3.0" `isSuffixOf`)
		. words <$> readProcess "pkg-config" ["--cflags", "gdk-3.0"] ""
	writeFile dstFile . unlines . map ((!! 1) . words)
		. filter ("#define GDK_KEY_" `isPrefixOf`) . lines =<< readFile fp
