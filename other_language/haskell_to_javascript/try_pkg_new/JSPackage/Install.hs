{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module JSPackage.Install (install) where

import JSPackage.Directory
import JSPackage.ReadConf

install :: [String] -> IO ()
install ars = do
	dp <- processArgs ars
	conf <- readConf dp
	print conf
	ld <- libraryDirectory conf
	putStrLn ld
	createDirectoryIfMissing ld
	let	ar = archivePath dp conf
		os = his dp conf
	copy (ar : os) ld
