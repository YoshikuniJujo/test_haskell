{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Data.Ratio
import Data.Word
import System.Environment
import System.Directory
import System.FilePath

rootDir :: IO FilePath
rootDir = (</> ".yoshj/lifegame") <$>  getHomeDirectory

main :: IO ()
main = do
	fp : _ <- getArgs
	rd <- rootDir
	createDirectoryIfMissing True rd
	createDirectoryIfMissing False $ rd </> "pngs"
	(nm, _, _, _, _, _, _, _, _, _) <- readLifegame . lines <$> readFile fp
	createDirectoryIfMissing False $ rd </> "pngs" </> nm
	print nm

readLifegame :: [String] ->
	(String, Int, Int, Int, Int, Int, Int, Int, Ratio Word16, [String])
readLifegame src = case words <$> src of
	["id:", nm] :
		["ratio:", rt] :
		["width:", w] :
		["height:", h] :
		["x-offset:", xo] :
		["y-offset:", yo] :
		["first-frame:", drp] :
		["frame-number:", fn] :
		["delay:", dly] :
		["shape:"] : shp -> (
		nm,
		read rt, read w, read h, read xo, read yo, read drp, read fn,
		read dly, head <$> shp )

	_ -> error "bad"
