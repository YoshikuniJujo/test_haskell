{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module JSPackage.Place (place) where

import JSPackage.Directory
import JSPackage.ReadConf

place :: [String] -> IO ()
place ars = do
	dp <- processArgs ars
	conf <- readConf dp
	ld <- libraryDirectory conf
	createDirectoryIfMissing ld
	print $ his dp conf
	copy (archivePath dp conf : his dp conf) ld
