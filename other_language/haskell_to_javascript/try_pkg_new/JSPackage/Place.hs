{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module JSPackage.Place (place) where

import JSPackage.Directory
import JSPackage.ReadConf

place :: [String] -> IO ()
place ars = do
	dp <- processArgs ars
	conf <- readConf dp
	print conf
	ld <- libraryDirectory conf
	putStrLn ld
	createDirectoryIfMissing ld
	let	ar = archivePath dp conf
		os = his dp conf
	copy (ar : os) ld
