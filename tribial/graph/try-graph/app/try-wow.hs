{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings, TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Data.Traversable
import System.Environment

import SortResultHason
import SortGraph

main :: IO ()
main = do
	fps <- getArgs
	let	hd0 = ["quick", "merge"]
	als <- for fps \fp -> do
		al <- readAll <$> readFile fp
		let	hd = header al
		print $ machine al
		print hd
		when (hd /= hd0) $ error "no mutch data"
		pure al
	mid <- readFile "/etc/machine-id"
	mmy <- lines <$> readFile ("settings/try-wow/" ++ init mid)
	let	[minY, maxY] = read <$> mmy
	mkGraph "try-wow.png" minY maxY hd0 als
