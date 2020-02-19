{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Control.Arrow
import Control.Monad
import Data.List
import System.Directory
import System.FilePath

purstAndPurs :: FilePath -> IO [(FilePath, FilePath)]
purstAndPurs d = do
	fps <- map (((d </>) . (<.> "purst") &&& (d </>) . (<.> "purs")) . dropExtension)
		. filter (isExtensionOf "purst") <$> getDirectoryContents d
	(fps ++) <$> (purstAndPursDirs =<< getSubDirectories d)

purstAndPursDirs :: [FilePath] -> IO [(FilePath, FilePath)]
purstAndPursDirs ds = concat <$> purstAndPurs `mapM` ds

getSubDirectories :: FilePath -> IO [FilePath]
getSubDirectories d = do
	fps <- map (d </>) . filter (not . ("." `isPrefixOf`)) <$> getDirectoryContents d
	filterM doesDirectoryExist fps
