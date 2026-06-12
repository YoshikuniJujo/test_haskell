{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module JSPackage.Register (register) where

import System.Environment
import System.Process

import JSPackage.ReadConf

register :: IO ()
register = do
	dp <- processArgs =<< getArgs
	conf <- readConf dp
	let	Just cp = confPath dp conf
		pr = proc "javascript-unknown-ghcjs-ghc-pkg-9.12.4"
			["register", cp]
	print pr
	putStrLn =<< readCreateProcess pr ""
