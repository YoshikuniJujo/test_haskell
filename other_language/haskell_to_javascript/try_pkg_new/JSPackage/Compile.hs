{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module JSPackage.Compile (compile) where

import System.Environment
import System.Process

import JSPackage.ReadConf

compile :: IO ()
compile = do
	dp <- processArgs =<< getArgs
	conf <- readConf dp
	let	Just nm = packageName conf
		Just vsn = packageVersion conf
		pn = nm ++ "-" ++ vsn ++ "-inplace"
	let	mds = modules dp conf
	print . proc "javascript-unknown-ghcjs-ghc-9.12.4"
		$ ["-package-name", pn] ++ mds
	(putStrLn =<<) . (`readCreateProcess` "")
		. proc "javascript-unknown-ghcjs-ghc-9.12.4"
		$ ["-package-name", pn] ++ mds
