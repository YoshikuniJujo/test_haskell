{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module JSPackage.Unregister (unregister) where

import System.Environment
import System.Process

import JSPackage.ReadConf

unregister :: IO ()
unregister = do
	dp <- processArgs =<< getArgs
	conf <- readConf dp
	let	Just cp = packageName conf
		pr = proc "javascript-unknown-ghcjs-ghc-pkg-9.12.4"
			["unregister", cp]
	print pr
	putStrLn =<< readCreateProcess pr ""
