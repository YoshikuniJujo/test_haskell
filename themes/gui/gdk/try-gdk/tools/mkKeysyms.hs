{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.List
import Data.Char
import System.Process

dstFile :: FilePath
dstFile = "./src/Graphics/Gdk/Events/GdkKeySyms.hsc"

main :: IO ()
main = do
	fp <- (++ "/gdk/gdkkeysyms.h") . drop 2 . head
		. filter ("gtk-3.0" `isSuffixOf`)
		. words <$> readProcess "pkg-config" ["--cflags", "gdk-3.0"] ""
	putStr . unlines . map (appHead toUpper . camelize . (!! 1) . words)
		. filter ("#define GDK_KEY_" `isPrefixOf`) . lines =<< readFile fp

appHead :: (a -> a) -> [a] -> [a]
appHead _ [] = []
appHead f (x : xs) = f x : xs

camelize :: String -> String
camelize "" = ""
camelize ('_' : c : cs) = toUpper c : camelize cs
camelize (c : cs) = toLower c : camelize cs
