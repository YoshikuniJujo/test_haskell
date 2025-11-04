{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Data.Ratio
import Data.Word
import System.Environment
import System.Directory
import System.FilePath

import Data.Image.Gray1 qualified as Img
import System.File.Png.Gray1.NoInterlace qualified as Png
import Lifegame.Words qualified as Lg

import Data.Lifegame.Glider qualified as Glider

rootDir :: IO FilePath
rootDir = (</> ".yoshj/lifegame") <$>  getHomeDirectory

main :: IO ()
main = do
	fp : _ <- getArgs
	rd <- rootDir
	createDirectoryIfMissing True rd
	createDirectoryIfMissing False $ rd </> "pngs"
	(nm, _, w, h, xo, yo, ff, cf, _, shp, gls) <- readLifegame . lines <$> readFile fp
	let	nmd = rd </> "pngs" </> nm
		szost = show w ++ "x" ++ show h ++ "_" ++ show xo ++ "x" ++ show yo
		szostd = nmd </> szost
	createDirectoryIfMissing True szostd
	print nm
	putStrLn szost
	let	b0 = Glider.addGs (Lg.putShapeAscii w h xo yo shp) gls
		bs = take cf $ drop ff $ Glider.boards' b0
		fps = (szostd </>) . boardName <$> [ff .. ff + cf]
		imgs = Lg.boardToGray1 <$> bs
--	Img.printAsAscii `mapM_` imgs
	zipWithM_ Png.write fps imgs

readLifegame :: [String] ->
	(String, Int, Int, Int, Int, Int, Int, Int, Ratio Word16, [String], [(Glider.G, (Int, Int))])
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
		["shape:"] : shpgls -> case span (not.null) shpgls of
			(shp, Glider.readGliders -> Just gls) -> (
				nm,
				read rt, read w, read h, read xo, read yo, read drp, read fn,
				read dly, head <$> shp, gls)
			(shp, egls) -> error $ "bad: " ++ show egls

	_ -> error "bad"

boardName :: Int -> FilePath
boardName n = "board_" <> replicate (4 - length s) '0' <> s <.> "png"
	where s = show n
