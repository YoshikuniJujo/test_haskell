{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module JSPackage.Install (install) where

import Control.Monad
import System.Environment
import System.Directory
import System.FilePath

import JSPackage.ReadConf

install :: [String] -> IO ()
install ars = do
	dp <- processArgs ars
	conf <- readConf dp
	print conf
	ld <- libraryDirectory conf
	putStrLn ld
	createDirectoryIfMissing False ld
	let	ar = archivePath dp conf
		os = his dp conf
	copy (ar : os) ld

copy :: [FilePath] -> FilePath -> IO ()
copy fs dr = zipWithM_ copyFile fs (mkDistFilePath fs dr)

mkDistFilePath :: [FilePath] -> FilePath -> [FilePath]
mkDistFilePath fs dr = (dr </>) . snd . splitFileName <$> fs
