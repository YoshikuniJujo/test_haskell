{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Data.Foldable
import Data.List
import System.Environment
import System.Directory

main :: IO ()
main = do
	fps <- getArgs
	for_ fps \fp -> do
		cnt <- readFile fp
		writeFile (fp ++ ".tmp") $ conv cnt
		renameFile (fp ++ ".tmp") fp

conv :: String -> String
conv str@(c : cs)
	| pre `isPrefixOf` str = post ++ drop (length pre) str
	| otherwise = c : conv cs
conv "" = ""

pre, post :: String
pre = "import Data.HeteroList"
post = "import Data.HeteroParList"
