{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module JSPackage.Hide (hide) where

import System.Process

import JSPackage.ReadConf

hide :: [String] -> IO ()
hide ars = do
	dp <- processArgs ars
	conf <- readConf dp
	let	Just cp = packageName conf
		pr = proc "javascript-unknown-ghcjs-ghc-pkg-9.12.4"
			["hide", cp]
	print pr
	putStrLn =<< readCreateProcess pr ""
