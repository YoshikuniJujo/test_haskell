{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ImplicitParams #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import System.Environment
import Stopgap.System.GLib.File
import Stopgap.System.GLib.Error.Io qualified as G.Error.Io

main :: IO ()
main = do
	let	?makeEFuns = [G.Error.Io.mkEFun]
--	let	?makeEFuns = []
	fp : _ <- getArgs
	f <- newForPath fp
	print =<< loadContents f Nothing
