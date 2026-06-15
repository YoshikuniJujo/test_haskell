{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module JSPackage.Archive (archive) where

import System.Process
import System.FilePath

import JSPackage.ReadConf qualified as C

archive :: [String] -> IO ()
archive ars = do
	dp <- C.processArgs ars
	conf <- C.readConf dp
	let	Just nm = C.packageName conf
		Just vsn = C.packageVersion conf
		pn = nm ++ "-" ++ vsn ++ "-inplace"
	let	mds = C.modules dp conf
		objs = C.objs dp conf
	print
		. proc "emar"
		$ ["qcls"] ++ (uncurry (</>) (C.archivePath dp conf) : objs)
	(putStrLn =<<) . (`readCreateProcess` "")
		. proc "emar"
		$ ["qcls"] ++ (uncurry (</>) (C.archivePath dp conf) : objs)
