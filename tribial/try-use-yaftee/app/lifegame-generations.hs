{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-x-partial #-}

module Main (main) where

import Control.Monad
import System.Environment
import System.Directory
import System.FilePath
import System.File.Png.Gray1.NoInterlace qualified as Png
import Lifegame.Board qualified as L

rootDir :: IO FilePath
rootDir = (</> ".yoshj/lifegame") <$> getHomeDirectory

main :: IO ()
main = do
	rd <- rootDir
	fp : _ <- getArgs
	(nm, w, h, x, y, d, t, s) <- readLifegame . lines <$> readFile fp
	let	fps = (dir </>) . boardName <$> [d .. d + t - 1]
		dir = rd </> "pngs" </> nm </> so
		so = show w ++ "x" ++ show h ++ "_" ++ show x ++ "x" ++ show y
		imgs = L.toGray1 <$> gs
		gs = take t . drop d $ L.generations $ L.putShapeAscii w h x y s
	createDirectoryIfMissing True dir
	zipWithM_ Png.write fps imgs

readLifegame :: [String] -> (String, Int, Int, Int, Int, Int, Int, [String])
readLifegame src = case words <$> src of
	["id:", nm] : ["width:", w] : ["height:", h] :
		["x-offset:", xo] : ["y-offset:", yo] :
		["first-frame:", drp] : ["frame-number:", fn] :
		["shape:"] : sp -> (
			nm, read w, read h, read xo, read yo,
			read drp, read fn, head <$> sp )
	_ -> error "bad"

boardName :: Int -> FilePath
boardName n = "board_" <> replicate (4 - length s) '0' <> s <.> "png"
	where s = show n
