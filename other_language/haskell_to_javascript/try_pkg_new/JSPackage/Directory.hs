{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module JSPackage.Directory where

import Control.Monad
import System.Directory qualified as D
import System.FilePath
import JSPackage.Values

createDirectoryIfMissing :: FilePath -> IO ()
createDirectoryIfMissing dr = do
	b <- D.doesDirectoryExist dr
	when b do
		b' <- checkDirectory dr
		when (not b') $ error "This directory is not under the control."
	createDirectoryIfMissing' dr

createDirectoryIfMissing' :: FilePath -> IO ()
createDirectoryIfMissing' dr = do
	b <- D.doesDirectoryExist dr
	when (not b) do
		D.createDirectory dr
		D.createDirectory $ dr </> ".yjspkg"
		writeFile (dr </> ".yjspkg/uuid") (underTheControl ++ "\n")

copy :: [FilePath] -> FilePath -> IO ()
copy fs dr = do
	b <- checkDirectory dr
	when (not b) $ error "This directory is not under the control."
	zipWithM_ copyFile fs (mkDistFilePath fs dr)

copy' :: FilePath -> [FilePath] -> FilePath -> IO ()
copy' sdr fs dr = do
	b <- checkDirectory dr
	when (not b) $ error "This directory is not under the control."
	zipWithM_ copyFile' ((sdr </>) <$> fs) (mkDistFilePath' fs dr)

copyFile :: FilePath -> FilePath -> IO ()
copyFile = D.copyFile

copyFile' :: FilePath -> FilePath -> IO ()
copyFile' src dst = do
	let	(ddr, _) = splitFileName dst
	D.createDirectoryIfMissing True ddr
	D.copyFile src dst

mkDistFilePath :: [FilePath] -> FilePath -> [FilePath]
mkDistFilePath fs dr = (dr </>) . snd . splitFileName <$> fs

mkDistFilePath' :: [FilePath] -> FilePath -> [FilePath]
mkDistFilePath' fs dr = (dr </>) <$> fs

chomp :: String -> String
chomp s = case last s of '\n' -> init s; _ -> s

checkDirectory :: FilePath -> IO Bool
checkDirectory dr =
	(== underTheControl) . chomp <$> readFile (dr </> ".yjspkg/uuid")

removeDirectoryRecursive :: FilePath -> IO ()
removeDirectoryRecursive dr = do
	b <- checkDirectory dr
	if b
	then D.removeDirectoryRecursive dr
	else error "This directory is not under the control."
