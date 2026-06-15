{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module JSPackage.Clean (clean) where

import System.FilePath
import System.Process

import JSPackage.ReadConf

clean :: [String] -> IO ()
clean ars = do
	dp <- processArgs ars
	conf <- readConf dp
	let	mds = modules dp conf
		ar = uncurry (</>) $ archivePath dp conf
		Just cnf = confPath dp conf
	putStrLn =<< readCreateProcess
		(proc "rm" $ "-f" : cnf : ar : filesToRemove mds) ""

filesToRemove :: [FilePath] -> [FilePath]
filesToRemove = ((\fp -> [fp -<.> "hi", fp -<.> "o"]) =<<)
