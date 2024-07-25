{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module MkPackageConf.RealMain (realMain, MetaData(..)) where


import Distribution.InstalledPackageInfo
import Distribution.Types.PackageId
import Distribution.Types.PackageName
import Distribution.Types.UnitId
import Distribution.Version
import Distribution.ModuleName
import Data.List qualified as L
import System.Environment
import System.FilePath

realMain :: MetaData -> IO ()
realMain md = do
	hm <- getEnv "HOME"
	tdir <- mkPkgDir . head . lines <$> readFile (hm </> ".local/ghc/etc/ghc-version.conf")
	putStr . showInstalledPackageInfo
		$ packageInfo tdir
			(mdPkgName md) (mdPkgVersion md) (mdExposedModules md)
			(mkDepends' $ mdDepends md)

data MetaData = MetaData {
	mdPkgName :: Name,
	mdPkgVersion :: Vsn,
	mdExposedModules :: [Module],
	mdDepends :: [(Name, Vsn)] }
	deriving Show

type Name = String
type Vsn = String
type Module = String

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

mkDepends' :: [(String, String)] -> [UnitId]
mkDepends' = map mkDepend'

mkDepend' :: (String, String) -> UnitId
mkDepend' (nm, vsn) = mkUnitId $ nm ++ "-" ++ vsn ++ "-inplace"

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
