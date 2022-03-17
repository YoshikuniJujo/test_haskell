{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.List
import Data.Maybe
import Data.Char

main :: IO ()
main = do
	lns <- lines <$> readFile "/usr/include/shaderc/shaderc.h"
	mapM_ putStrLn . fromJust . (
			map (dropEqual . dropComma)
				. filter (not . null)
				. filter (not . isComment) <$> )
		. lookup "shaderc_shader_kind" $ typedefEnums lns

typedefEnums, typedefEnumsTail :: [String] -> [(String, [String])]
typedefEnums [] = []
typedefEnums ("typedef enum {" : lns) = typedefEnumsTail lns
typedefEnums (_ : lns) = typedefEnums lns

typedefEnumsTail (ln : lns)
	| Just n <- getTail ln = (n, []) : typedefEnums lns
	| otherwise = (nm, ln : es) : nmess
	where (nm, es) : nmess = typedefEnumsTail lns
typedefEnumsTail [] = error "bad"

getTail :: String -> Maybe String
getTail ('}' : ' ' : s) = case (init s, last s) of
	(nm, ';') -> Just nm
	_ -> Nothing
getTail _ = Nothing

isComment :: String -> Bool
isComment (dropWhile isSpace -> ln) = "//" `isPrefixOf` ln

dropComma :: String -> String
dropComma (dropWhile isSpace -> e) = case (init e, last e) of
	(e', ',') -> e'
	_ -> e

dropEqual :: String -> String
dropEqual e = dropWhile isSpace $ takeWhile (/= '=') e

{-
sep :: Eq a => a -> [a] -> [[a]]
sep _ [] = []
sep s (x : xs)
	| x == s = [] : sep s xs
	| otherwise =
		(
		-}
