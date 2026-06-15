{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module JSPackage.Unplace (unplace) where

import JSPackage.Directory
import JSPackage.ReadConf

unplace :: [String] -> IO ()
unplace ars = do
	dp <- processArgs ars
	conf <- readConf dp
	ld <- libraryDirectory conf
	removeDirectoryRecursive ld
