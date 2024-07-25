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

import Hason
import Hason.Eval

main :: IO ()
main = do
	[pkgfp] <- getArgs
	Right hsn <- eval <$> readFile pkgfp
	let	Just md = hasonToMetaData hsn
	tdir <- mkPkgDir . head . lines <$> readFile "ghc-version.conf"
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

hasonToMetaData :: Hason -> Maybe MetaData
hasonToMetaData h = do
	nm <- toName =<< lookup (KStr "name") h
	vsn <- toVersion =<< lookup (KStr "version") h
	ems <- toModules =<< lookup (KStr "exposed-modules") h
	dps <- toNameVsns =<< lookup (KStr "depends") h
	pure $ MetaData nm vsn ems dps

toName :: HasonValue -> Maybe Name
toName = \case Str nm -> Just nm; _ -> Nothing

toVersion :: HasonValue -> Maybe Vsn
toVersion = \case Str vsn -> Just vsn; _ -> Nothing

toModules :: HasonValue -> Maybe [Module]
toModules = \case
	Seq ms -> (\case Str mn -> Just mn; _ -> Nothing) `mapM` ms
	_ -> Nothing

toNameVsns :: HasonValue -> Maybe [(Name, Vsn)]
toNameVsns = \case
	Seq ds -> mapM toNameVsn ds
	_ -> Nothing

toNameVsn :: HasonValue -> Maybe (Name, Vsn)
toNameVsn = \case
	Dct p -> do
		Str n <- lookup (KStr "name") p
		Str v <- lookup (KStr "version") p
		pure (n, v)
	_ -> Nothing

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

mkDepends' :: [(String, String)] -> [UnitId]
mkDepends' = map mkDepend'

mkDepend' :: (String, String) -> UnitId
mkDepend' (nm, vsn) = mkUnitId $ nm ++ "-" ++ vsn ++ "-inplace"

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
