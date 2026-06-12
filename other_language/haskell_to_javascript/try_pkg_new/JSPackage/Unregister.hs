{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module JSPackage.Unregister (unregister) where

import System.Process

import JSPackage.ReadConf

unregister :: [String] -> IO ()
unregister ars = do
	dp <- processArgs ars
	conf <- readConf dp
	let	Just cp = packageName conf
		pr = proc "javascript-unknown-ghcjs-ghc-pkg-9.12.4"
			["unregister", cp]
	print pr
	putStrLn =<< readCreateProcess pr ""
