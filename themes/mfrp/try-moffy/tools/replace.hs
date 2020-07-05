{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Data.List
import System.Environment

main :: IO ()
main = do
	src : dst : fp : [] <- getArgs
	cnt <- replace src dst <$> readFile fp
	writeFile (fp ++ ".rpl") cnt

replace :: String -> String -> String -> String
replace _ _ "" = ""
replace s d str@(c : cs)
	| s `isPrefixOf` str = d ++ replace s d (drop (length s) str)
	| otherwise = c : replace s d cs
