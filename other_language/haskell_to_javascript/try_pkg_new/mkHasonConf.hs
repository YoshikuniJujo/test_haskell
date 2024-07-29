{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import MkPackageConf.RealMain

main :: IO ()
main = realMain MetaData {
	mdPkgName = "hason",
	mdPkgVersion = "0.1.0.0",
	mdExposedModules = ["Hason", "Hason.Eval"],
	mdDepends = [("array", "0.5.7.0"), ("base", "4.20.0.0")] }
