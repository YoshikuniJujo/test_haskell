{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.List
import Data.Char
import Text.Nowdoc
import System.Process

dstFile :: FilePath
dstFile = "./src/Graphics/Gdk/Events/GdkKeySyms.hsc"

header :: String
header = [nowdoc|
{-

File auto-generated from script tools/mkGdkKeySyms.hs using the input file
/usr/include/gtk-3.0/gdk/gdkkeysyms.h

-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Gdk.Events.GdkKeySyms where

import Foreign.C.Types
import Foreign.C.Enum

#include <gdk/gdk.h>

enum "GdkKeySym" ''CUInt [''Show] [
|]

main :: IO ()
main = do
	fp <- (++ "/gdk/gdkkeysyms.h") . drop 2 . head
		. filter ("gtk-3.0" `isSuffixOf`)
		. words <$> readProcess "pkg-config" ["--cflags", "gdk-3.0"] ""
	putStr . (header ++) . unlines . appLast ((++ " ]") . init) . map (mkLine . (!! 1) . words)
		. filter ("#define GDK_KEY_" `isPrefixOf`) . lines =<< readFile fp
	writeFile dstFile . (header ++) . unlines . appLast ((++ " ]") . init) . map (mkLine . (!! 1) . words)
		. filter ("#define GDK_KEY_" `isPrefixOf`) . lines =<< readFile fp

mkLine :: String -> String
mkLine c =
	"\t(\"" ++ appHead toUpper (camelize c) ++ "\", " ++
	"#{const " ++ c ++ "}),"

appHead :: (a -> a) -> [a] -> [a]
appHead _ [] = []
appHead f (x : xs) = f x : xs

appTail :: (a -> a) -> [a] -> [a]
appTail _ [] = []
appTail f (x : xs) = x : map f xs

appLast :: (a -> a) -> [a] -> [a]
appLast _ [] = []
appLast f [x] = [f x]
appLast f (x : xs) = x : appLast f xs

camelize :: String -> String
camelize = camelize' . separateUnderscore

camelize' :: [String] -> String
camelize' ss = concatMap (appHead toUpper . appTail toLower) ss' ++ concatMap ("_" ++) s
	where ss' = take 2 ss; s = drop 2 ss

separateUnderscore :: String -> [String]
separateUnderscore "" = [""]
separateUnderscore ('_' : cs) = "" : separateUnderscore cs
separateUnderscore (c : cs) = appHead (c :) $ separateUnderscore cs
