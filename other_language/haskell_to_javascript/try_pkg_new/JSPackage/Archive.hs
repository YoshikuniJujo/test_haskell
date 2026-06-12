{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module JSPackage.Archive (archive) where

import System.Environment
import System.Process

import JSPackage.ReadConf qualified as C

archive :: IO ()
archive = do
	dp <- C.processArgs =<< getArgs
	conf <- C.readConf dp
	let	Just nm = C.packageName conf
		Just vsn = C.packageVersion conf
		pn = nm ++ "-" ++ vsn ++ "-inplace"
	let	mds = C.modules dp conf
		objs = C.objs dp conf
	print
		. proc "emar"
		$ ["qcls"] ++ (C.archivePath dp conf : objs)
	(putStrLn =<<) . (`readCreateProcess` "")
		. proc "emar"
		$ ["qcls"] ++ (C.archivePath dp conf : objs)
	print $ C.objs dp conf
	putStrLn $ C.archivePath dp conf
