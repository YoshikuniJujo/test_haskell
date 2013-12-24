import Distribution.Simple.LocalBuildInfo(
	LocalBuildInfo, CopyDest(..), absoluteInstallDirs)
import qualified Distribution.Simple.LocalBuildInfo as LBI
import Control.Applicative

import Distribution.PackageDescription.Parse
import Distribution.PackageDescription
import Distribution.Verbosity

import Distribution.Package
import Distribution.Version

import System.Directory
import Data.List

import System.Environment

data InstallDirs = InstallDirs {
	bindir :: FilePath,
	libdir :: FilePath,
	datadir :: FilePath,
	htmldir :: FilePath
 } deriving Show

getLocalBuildInfo :: IO LocalBuildInfo
getLocalBuildInfo = read . (!! 1) . lines <$> readFile "./dist/setup-config"

getPackageDescription :: IO PackageDescription
getPackageDescription = packageDescription <$>
	(readPackageDescription silent =<< getCabalFile)

getCabalFile :: IO FilePath
getCabalFile =
	head . filter (".cabal" `isSuffixOf`) <$> getDirectoryContents "."

getInstallDirs :: IO InstallDirs
getInstallDirs = do
	lbi <- getLocalBuildInfo
	pd <- getPackageDescription
	let dirs = absoluteInstallDirs pd lbi NoCopyDest
	return $ InstallDirs {
		bindir = LBI.bindir dirs,
		libdir = LBI.libdir dirs,
		datadir = LBI.datadir dirs,
		htmldir = LBI.htmldir dirs
	 }

showInstallDirsGen :: (FilePath -> String) -> InstallDirs -> String
showInstallDirsGen sw InstallDirs {
	bindir = bd,
	libdir = ld,
	datadir = dd,
	htmldir = hd } =
	"bindir : " ++ sw bd ++ "\n" ++
	"libdir : " ++ sw ld ++ "\n" ++
	"datadir: " ++ sw dd ++ "\n" ++
	"htmldir: " ++ sw hd ++ "\n"

showInstallDirs :: InstallDirs -> String
showInstallDirs = showInstallDirsGen id

showRPInstallDirs :: FilePath -> InstallDirs -> String
showRPInstallDirs = showInstallDirsGen . removePrefix

printInstallDirs :: InstallDirs -> IO ()
printInstallDirs = putStr . showInstallDirs

printRPInstallDirs :: FilePath -> InstallDirs -> IO ()
printRPInstallDirs p = putStr . showRPInstallDirs p

removePrefix :: FilePath -> FilePath -> String
removePrefix p s
	| p `isPrefixOf` s = dropWhile (== '/') $ drop (length p) s
	| otherwise = s

main :: IO ()
main = do
	args <- getArgs
	let prfx = case args of
		[] -> ""
		[p] -> p
		_ -> error "bad arguments"
	getInstallDirs >>= printRPInstallDirs prfx
