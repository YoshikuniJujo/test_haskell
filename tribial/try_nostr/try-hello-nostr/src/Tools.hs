{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Tools where

import Data.Function
import Data.Bool
import Data.Char
import Data.Text qualified as T
import Numeric

strToHexStr :: String -> String
strToHexStr = concat . (sh <$>) . map ord
	where
	sh n = let s = showHex n "" in replicate (2 - length s) '0' ++ s

separate :: Int -> String -> [String]
separate _ "" = []
separate n s = take n s : separate n (drop n s)

chomp :: T.Text -> T.Text
chomp t = if T.last t == '\n' then T.init t else t

doWhile :: IO Bool -> IO ()
doWhile = fix \go -> (=<<) <$> bool (pure ()) . go <*> id
