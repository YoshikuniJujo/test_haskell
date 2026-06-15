{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module JSPackage.ReadConf (
	processArgs, readConf,
	packageName, packageVersion,
	exposedModules, modules, objs, his, his',
	archivePath, libraryDirectory,
	hasonToMetaData, MetaData(..),
	confPath
	) where

import Data.Maybe
import Data.Char
import System.Environment
import System.Directory
import System.FilePath

import Hason
import Hason.Eval

processArgs :: [String] -> IO FilePath
processArgs [] = getCurrentDirectory
processArgs [dp] = (</> toCamel dp) <$> getCurrentDirectory
processArgs _ = error "bad"

toCamel :: String -> String
toCamel "" = ""
toCamel ('-' : c : cs) = toUpper c : toCamel cs
toCamel (c : cs) = c : toCamel cs

readConf :: FilePath -> IO Hason
readConf dp = do
	Right conf <- eval <$> readFile (dp </> hasonName (takeBaseName dp))
	pure conf

packageName :: Hason -> Maybe String
packageName cnf = (\(Str s) -> s) <$> lookup (KStr "name") cnf

packageVersion :: Hason -> Maybe String
packageVersion cnf = (\(Str s) -> s) <$> lookup (KStr "version") cnf

exposedModules :: FilePath -> Hason -> [FilePath]
exposedModules dp cnf = let
	Seq emds = fromMaybe (Seq []) $ lookup (KStr "exposed-modules") cnf in
	((dp </> "src") </>) . moduleNameToFilePath' <$> emds

exposedModules' :: FilePath -> Hason -> (FilePath, [FilePath])
exposedModules' dp cnf = let
	Seq emds = fromMaybe (Seq []) $ lookup (KStr "exposed-modules") cnf in
	(dp </> "src" ,) $ moduleNameToFilePath' <$> emds

modules :: FilePath -> Hason -> [FilePath]
modules dp cnf = let
	Seq omds = fromMaybe (Seq []) $ lookup (KStr "other-modules") cnf
	omds' = ((dp </> "src") </>) . moduleNameToFilePath' <$> omds in
	exposedModules dp cnf ++ omds'

modules' :: FilePath -> Hason -> (FilePath, [FilePath])
modules' dp cnf = let
	Seq omds = fromMaybe (Seq []) $ lookup (KStr "other-modules") cnf
	omds' = moduleNameToFilePath' <$> omds in
	(dp </> "src", snd (exposedModules' dp cnf) ++ omds')

objs :: FilePath -> Hason -> [FilePath]
objs dp cnf = (-<.> "o") <$> modules dp cnf

his :: FilePath -> Hason -> [FilePath]
his dp cnf = (-<.> "hi") <$> modules dp cnf

his' :: FilePath -> Hason -> (FilePath, [FilePath])
his' dp cnf = ((-<.> "hi") <$>) <$> modules' dp cnf

archivePath :: FilePath -> Hason -> (FilePath, FilePath)
archivePath dp cnf = let
	Just nm = packageName cnf
	Just vsn = packageVersion cnf in
	(dp, "libHS" ++ nm ++ "-" ++ vsn ++ "-inplace.a")

moduleNameToFilePath' :: HasonValue -> FilePath
moduleNameToFilePath' (Str s) = periodToSlash s <.> "hs"

periodToSlash :: String -> FilePath
periodToSlash "" = ""
periodToSlash ('.' : cs) = '/' : periodToSlash cs
periodToSlash (c : cs) = c : periodToSlash cs

hasonName :: FilePath -> String
hasonName = (++ ".hason") . capitalize . toCamel . takeBaseName

capitalize :: String -> String
capitalize "" = ""
capitalize (c : cs) = toUpper c : cs

libraryDirectory :: Hason -> IO FilePath
libraryDirectory cnf = do
	let	Just nm = packageName cnf
		Just vsn = packageVersion cnf
	hd <- getEnv "HOME"
	pure $ hd </>
		".local/ghc-9.12.4/lib/javascript-ghcjs-ghc-9.12.4-inplace" </>
		nm ++ "-" ++ vsn ++ "-inplace"

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

data MetaData = MetaData {
	mdPkgName :: Name,
	mdPkgVersion :: Vsn,
	mdExposedModules :: [Module],
	mdDepends :: [(Name, Vsn)] }
	deriving Show

confPath :: FilePath -> Hason -> Maybe FilePath
confPath dp conf = (dp </>) . (<.> "conf") <$> packageName conf
