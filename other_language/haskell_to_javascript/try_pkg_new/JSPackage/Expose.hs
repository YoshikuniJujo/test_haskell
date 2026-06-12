{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module JSPackage.Expose (expose) where

import System.Process

import JSPackage.ReadConf

expose :: [String] -> IO ()
expose ars = do
	dp <- processArgs ars
	conf <- readConf dp
	let	Just cp = packageName conf
		pr = proc "javascript-unknown-ghcjs-ghc-pkg-9.12.4"
			["expose", cp]
	print pr
	putStrLn =<< readCreateProcess pr ""
