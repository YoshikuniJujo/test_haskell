{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module JSPackage.MkConf (mkConf, printConf, realMain, MetaData(..)) where

import Distribution.InstalledPackageInfo qualified as Dstr
import Distribution.Types.PackageId
import Distribution.Types.PackageName
import Distribution.Types.UnitId
import Distribution.Version
import Distribution.ModuleName
import Data.List qualified as L
import System.Environment
import System.FilePath

import Hason
import JSPackage.ReadConf

mkConf :: IO ()
mkConf = do
	(cp, cc) <- pathAndCont
	writeFile cp cc

printConf :: IO ()
printConf = do
	(cp, cc) <- pathAndCont
	putStrLn cp
	putStr cc

pathAndCont :: IO (FilePath, String)
pathAndCont = do
	dp <- processArgs =<< getArgs
	conf <- readConf dp
	let	Just cp = confPath dp conf
	(cp ,) <$> getConfFromHason conf

getConf :: IO String
getConf = do
	dp <- processArgs =<< getArgs
	conf <- readConf dp
	getConfFromHason conf

getConfFromHason :: Hason -> IO String
getConfFromHason conf = 
	maybe (error "bad") getConfFromMeta (hasonToMetaData conf)

realMain :: MetaData -> IO ()
realMain md = do
	hm <- getEnv "HOME"
	tdir <- mkPkgDir . head . lines <$> readFile (hm </> ".local/ghc/etc/ghc-version.conf")
	putStr . Dstr.showInstalledPackageInfo
		$ packageInfo tdir
			(mdPkgName md) (mdPkgVersion md) (mdExposedModules md)
			(mkDepends' $ mdDepends md)

getConfFromMeta :: MetaData -> IO String
getConfFromMeta md = do
	hm <- getEnv "HOME"
	tdir <- mkPkgDir . head . lines <$> readFile (hm </> ".local/ghc/etc/ghc-version.conf")
	pure . Dstr.showInstalledPackageInfo
		$ packageInfo tdir
			(mdPkgName md) (mdPkgVersion md) (mdExposedModules md)
			(mkDepends' $ mdDepends md)

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
	"${pkgroot}/../lib/javascript-ghcjs-ghc-" ++ vsn ++ "-inplace" ++ "/"

mkDepends' :: [(String, String)] -> [UnitId]
mkDepends' = map mkDepend'

mkDepend' :: (String, String) -> UnitId
mkDepend' (nm, vsn) = mkUnitId $ nm ++ "-" ++ vsn ++ "-inplace"

packageInfo :: FilePath -> String -> String -> [String] -> [UnitId] -> Dstr.InstalledPackageInfo
packageInfo tdir pnm (mkPkgVersion -> pvsn) mds dpds = Dstr.emptyInstalledPackageInfo {
	Dstr.sourcePackageId = PackageIdentifier
		(mkPackageName pnm) (mkVersion pvsn),
	Dstr.installedUnitId = mkUnitId $ mkPkgId pnm pvsn,
	Dstr.compatPackageKey = mkPkgId pnm pvsn,
	Dstr.exposedModules = (`Dstr.ExposedModule` Nothing) <$> mkModules mds,
	Dstr.importDirs = [tdir </> mkPkgId pnm pvsn],
	Dstr.libraryDirs = [tdir </> mkPkgId pnm pvsn],
	Dstr.libraryDirsStatic = [tdir </> mkPkgId pnm pvsn],
	Dstr.hsLibraries = ["HS" ++ mkPkgId pnm pvsn],
	Dstr.depends = dpds }
