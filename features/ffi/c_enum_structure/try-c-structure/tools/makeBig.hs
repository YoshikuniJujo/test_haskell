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

struct "Big" 100 8 [
|] ++ intercalate ",\n" (map member [0 .. 99]) ++ [nowdoc|
	]
	[]
|]

member :: Int -> String
member i = "\t(\"some" ++ show i ++ "\", ''Int, [| peek . castPtr |], [| poke . castPtr |])"
