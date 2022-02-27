{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.List
import Text.Nowdoc

main :: IO ()
main = do
	putStrLn $ [nowdoc|
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Big where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.Struct

|] ++ intercalate "\n" (uncurry mkBig <$> [
		(1, 100), (2, 76), (3, 100), (4, 61), (5, 62), (6, 63)
		])

mkBig :: Int -> Int -> String
mkBig i n = "struct \"Big" ++ show i ++ "\" 100 8 [\n" ++
	intercalate ",\n" (map (member "some") [0 .. n - 1]) ++ "\n\t] []\n"

member :: String -> Int -> String
member nm i = "\t(\"" ++ nm ++ show i ++ "\", ''Int, [| peek . castPtr |], [| poke . castPtr |])"
