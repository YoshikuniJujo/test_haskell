{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Data.Lifegame.Glider qualified as Glider
import System.Environment
import System.File.Png.Lifegame
import Text.Read
import Lifegame.Words

main :: IO ()
main = do
	stts : fpo : _ <- getArgs
	Just (sz, (w, h), glds) <- readSettings <$> readFile stts
	{-
	writeBoard fpo
		(Glider.add (emptyBoard 10 10) 3 3
			(Glider.G Glider.Shape1 Glider.Right Glider.Down))
			-}
	writeGliders fpo w h glds sz

addGlider :: Board -> (Glider.G, (Int, Int)) -> Board
addGlider bd (gd, (x, y)) = Glider.add bd x y gd

writeGliders ::
	FilePath -> Int -> Int -> [(Glider.G, (Int, Int))] -> Int -> IO ()
writeGliders fpo w h gds sz =
	(flip $ writeBoard fpo) sz $ foldl addGlider (emptyBoard w h) gds

readSettings :: String -> Maybe (Int, (Int, Int), [(Glider.G, (Int, Int))])
readSettings src = do
	((sz, w, h), src'') <- readSize src'
	glds <- readGliders src''
	pure (sz, (w, h), glds)
	where
	src' = words <$> lines src

readSize :: [[String]] -> Maybe ((Int, Int, Int), [[String]])
readSize (["size:", sz_] : ["width:", wd_] : ["height:", hg_] : rst) = do
	sz <- readMaybe sz_
	wd <- readMaybe wd_
	hg <- readMaybe hg_
	pure ((sz, wd, hg), rst)
readSize _ = Nothing

readGliders :: [[String]] -> Maybe [(Glider.G, (Int, Int))]
readGliders [] = Just []
readGliders src = do
	let	src' = dropWhile null src
	(g1, src'') <- readGlider1 src'
	(g1 :) <$> readGliders src''

readGlider1 :: [[String]] -> Maybe ((Glider.G, (Int, Int)), [[String]])
readGlider1 (
	["shape:", spn] :
	["x-offset:", xo_] :
	["y-offset:", yo_] :
	["left-right:", lr_] :
	["up-down:", ud_] : rst) = do
	sp <- readMaybe $ "Shape" ++ spn
	xo <- readMaybe xo_
	yo <- readMaybe yo_
	lr <- readMaybe lr_
	ud <- readMaybe ud_
	pure ((Glider.G sp lr ud, (xo, yo)), rst)
readGlider1 _ = Nothing
