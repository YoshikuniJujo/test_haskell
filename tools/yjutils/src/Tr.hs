{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Tr where

import Control.Arrow
import Data.List
import System.Directory
import System.FilePath

rec :: String -> String -> FilePath -> IO ()
rec src dst fp = recursive_ (trFile src dst) fp

trFile :: String -> String -> FilePath -> IO ()
trFile src dst fp = do
	str <- readFile fp
	writeFile tmpFile $ tr src dst str
	copyFile tmpFile fp
	removeFile tmpFile
	where tmpFile = makeTmpFp fp

makeTmpFp :: FilePath -> FilePath
makeTmpFp fp = case splitPath fp of
	[] -> error "bad name"
	ss -> joinPath $ init ss ++ [".tmp" <.> last ss]

tr :: String -> String -> String -> String
tr _ _ "" = ""
tr src dst ca@(c : cs)
	| src `isPrefixOf` ca = dst ++ tr src dst (drop (length src) ca)
	| otherwise = c : tr src dst cs

recursive_ :: (FilePath -> IO ()) -> FilePath -> IO ()
recursive_ act fp = do
	(dirs, fls) <- getDirectoriesAndFiles fp
	act `mapM_` fls
	recursive_ act `mapM_` dirs

getDirectoriesAndFiles :: FilePath -> IO ([FilePath], [FilePath])
getDirectoriesAndFiles fp = directoriesAndFiles =<< ((fp </>) <$>)
	. filter (not . ("." `isPrefixOf`)) <$> getDirectoryContents fp

directoriesAndFiles :: [FilePath] -> IO ([FilePath], [FilePath])
directoriesAndFiles [] = pure ([], [])
directoriesAndFiles (fp : fps) = do
	dr <- doesDirectoryExist fp
	fl <- doesFileExist fp
	case (dr, fl) of
		(True, _) -> ((fp :) `first`) <$> directoriesAndFiles fps
		(_, True) -> ((fp :) `second`) <$> directoriesAndFiles fps
		_ -> directoriesAndFiles fps
