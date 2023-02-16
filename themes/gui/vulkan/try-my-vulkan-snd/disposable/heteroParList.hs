{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Data.Foldable
import Data.List
import System.Environment
import System.Directory

main :: IO ()
main = do
	pre : post : fps <- getArgs
	for_ fps \fp -> do
		cnt <- readFile fp
		writeFile (fp ++ ".tmp") $ conv pre post cnt
		renameFile (fp ++ ".tmp") fp

conv :: String -> String -> String -> String
conv pre post str@(c : cs)
	| pre `isPrefixOf` str = post ++ conv pre post (drop (length pre) str)
	| otherwise = c : conv pre post cs
conv _ _ "" = ""
