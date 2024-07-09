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
	[pnm, pvsn, emsfp, dpdsfp] <- getArgs
	tdir <- mkPkgDir . head . lines <$> readFile "ghc-version.conf"
	mds <- lines <$> readFile emsfp
	dpds <- mkDepends <$> readFile dpdsfp
	putStr . showInstalledPackageInfo $ packageInfo tdir pnm pvsn mds dpds

mkPkgVersion :: String -> [Int]
mkPkgVersion vstr = read <$> sepBy '.' vstr

sepBy :: Eq a => a -> [a] -> [[a]]
sepBy s = \case
	[] -> [[]]
	x : xs	| x == s -> [] : r
		| otherwise -> (x : h) : t
		where r@(h : t) = sepBy s xs

mkPkgId :: String -> [Int] -> String
mkPkgId nm vsn = nm ++ "-" ++ L.intercalate "." (show <$> vsn) ++ "-inplace"

mkModules :: [String] -> [ModuleName]
mkModules mstrs = fromString <$> mstrs

mkPkgDir :: String -> String
mkPkgDir vsn =
	"${pkgroot}/../lib/javascript-ghcjs-ghc-" ++ vsn ++ "/"

mkDepends :: String -> [UnitId]
mkDepends = ((mkDepend . words) <$>) . lines

mkDepend :: [String] -> UnitId
mkDepend (nm : vsn : _) = mkUnitId $ nm ++ "-" ++ vsn ++ "-inplace"
mkDepend _ = error "bad"

packageInfo :: FilePath -> String -> String -> [String] -> [UnitId] -> InstalledPackageInfo
packageInfo tdir pnm (mkPkgVersion -> pvsn) mds dpds = emptyInstalledPackageInfo {
	sourcePackageId = PackageIdentifier
		(mkPackageName pnm) (mkVersion pvsn),
	installedUnitId = mkUnitId $ mkPkgId pnm pvsn,
	compatPackageKey = mkPkgId pnm pvsn,
	exposedModules = (`ExposedModule` Nothing) <$> mkModules mds,
	importDirs = [tdir </> mkPkgId pnm pvsn],
	libraryDirs = [tdir </> mkPkgId pnm pvsn],
	libraryDirsStatic = [tdir </> mkPkgId pnm pvsn],
	hsLibraries = ["HS" ++ mkPkgId pnm pvsn],
	depends = dpds }
