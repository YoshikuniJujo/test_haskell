{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Distribution.InstalledPackageInfo
import Distribution.Types.PackageId
import Distribution.Types.PackageName
import Distribution.Types.UnitId
import Distribution.Version
import Distribution.ModuleName
import Data.List qualified as L
import System.Environment
import System.FilePath

main :: IO ()
main = do
	[pnm, pvsn, emsfp] <- getArgs
	tdir <- head . lines <$> readFile "packageTopDir.conf"
	mds <- lines <$> readFile emsfp
	putStr . showInstalledPackageInfo
		$ packageInfo tdir pnm pvsn mds

mkPkgVersion :: String -> [Int]
mkPkgVersion vstr = read <$> sepBy '.' vstr

sepBy :: Eq a => a -> [a] -> [[a]]
sepBy s = \case
	[] -> [[]]
	x : xs	| x == s -> [] : r
		| otherwise -> (x : h) : t
		where r@(h : t) = sepBy s xs

mkPkgId :: String -> [Int] -> String
mkPkgId nm vsn = nm ++ "-" ++ L.intercalate "." (show <$> vsn)

moduleStrs :: [String]
moduleStrs = ["Hello"]

mkModules :: [String] -> [ModuleName]
mkModules mstrs = fromString <$> mstrs

packageInfo :: FilePath -> String -> String -> [String] -> InstalledPackageInfo
packageInfo tdir pnm (mkPkgVersion -> pvsn) mds = emptyInstalledPackageInfo {
	sourcePackageId = PackageIdentifier
		(mkPackageName pnm) (mkVersion pvsn),
	installedUnitId = mkUnitId $ mkPkgId pnm pvsn,
	compatPackageKey = mkPkgId pnm pvsn,
	exposedModules = (`ExposedModule` Nothing) <$> mkModules mds,
	importDirs = [tdir </> mkPkgId pnm pvsn],
	libraryDirs = [tdir </> mkPkgId pnm pvsn],
	hsLibraries = ["HS" ++ mkPkgId pnm pvsn] }
