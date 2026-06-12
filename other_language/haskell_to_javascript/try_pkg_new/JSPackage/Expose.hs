{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module JSPackage.Expose (expose) where

import System.Environment
import System.Process

import JSPackage.ReadConf

expose :: IO ()
expose = do
	dp <- processArgs =<< getArgs
	conf <- readConf dp
	let	Just cp = packageName conf
		pr = proc "javascript-unknown-ghcjs-ghc-pkg-9.12.4"
			["expose", cp]
	print pr
	putStrLn =<< readCreateProcess pr ""
