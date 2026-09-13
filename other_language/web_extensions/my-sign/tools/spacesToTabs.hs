{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import System.Environment

main :: IO ()
main = do
	[read -> n] <- getArgs
	interact $ run n

run :: Int -> String -> String
run n = fromText n . toText

data Text = Spaces Int | LineFeed | Text String deriving Show

toText :: String -> [Text]
toText "" = []
toText ('\n' : str') = LineFeed : toText str'
toText str@(' ' : _) = Spaces (length ss) : toText str'
	where (ss, str') = span (== ' ') str
toText str = Text t : toText str'
	where (t, str') = span (`notElem` " \n") str

fromText :: Int -> [Text] -> String
fromText _ [] = ""
fromText m (LineFeed : Spaces n : str) = '\n' : spacesToTabs m n ++ fromText m str
fromText m (LineFeed : str) = '\n' : fromText m str
fromText m (Spaces n : str) = replicate n ' ' ++ fromText m str
fromText m (Text s : str) = s ++ fromText m str

spacesToTabs :: Int -> Int -> String
spacesToTabs m n = replicate t '\t' ++ replicate s ' '
	where
	(t, s) = n `divMod` m
