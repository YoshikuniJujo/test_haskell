{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module JSPackage.Clean (clean) where

import System.Environment
import System.FilePath
import System.Process

import JSPackage.ReadConf

clean :: IO ()
clean = do
	dp <- processArgs =<< getArgs
	conf <- readConf dp
	let	mds = modules dp conf
		ar = archivePath dp conf
		Just cnf = confPath dp conf
	putStrLn =<< readCreateProcess
		(proc "rm" $ "-f" : cnf : ar : filesToRemove mds) ""

filesToRemove :: [FilePath] -> [FilePath]
filesToRemove = ((\fp -> [fp -<.> "hi", fp -<.> "o"]) =<<)
