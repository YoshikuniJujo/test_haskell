{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module JSPackage.Hide (hide) where

import System.Environment
import System.Process

import JSPackage.ReadConf

hide :: IO ()
hide = do
	dp <- processArgs =<< getArgs
	conf <- readConf dp
	let	Just cp = packageName conf
		pr = proc "javascript-unknown-ghcjs-ghc-pkg-9.12.4"
			["hide", cp]
	print pr
	putStrLn =<< readCreateProcess pr ""
