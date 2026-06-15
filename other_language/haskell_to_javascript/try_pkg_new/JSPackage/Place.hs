{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module JSPackage.Place (place) where

import System.FilePath

import JSPackage.Directory
import JSPackage.ReadConf

place :: [String] -> IO ()
place ars = do
	dp <- processArgs ars
	conf <- readConf dp
	ld <- libraryDirectory conf
	createDirectoryIfMissing ld
	print $ his dp conf
	print $ his' dp conf
	copy [uncurry (</>) (archivePath dp conf)] ld
	uncurry copy' (his' dp conf) ld
