{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Distribution.InstalledPackageInfo
import Distribution.Types.PackageId
import Distribution.Types.PackageName
import Distribution.Types.UnitId
import Distribution.Types.ComponentId
import Distribution.Version
import Distribution.ModuleName

main :: IO ()
main = do
	putStr $ showInstalledPackageInfo packageInfo

packageInfo :: InstalledPackageInfo
packageInfo = emptyInstalledPackageInfo {
	sourcePackageId = PackageIdentifier
		(mkPackageName "yj-hello") (mkVersion [0, 1, 0, 0]),
	installedUnitId = mkUnitId "hello-0.1.0.0-inplace",
	compatPackageKey = "hello-0.1.0.0-inplace",
	installedComponentId_ = mkComponentId "foobar",
	exposedModules = [ExposedModule (fromComponents ["Hello"]) Nothing],
	importDirs = ["/home/tatsuya/tmp/hello-0.1.0.0-inplace"],
	libraryDirs = ["/home/tatsuya/tmp/hello-0.1.0.0-inplace"],
	hsLibraries = ["HShello-0.1.0.0-inplace"] }
