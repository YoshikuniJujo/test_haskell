{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Control.Arrow
import System.Directory
import System.FilePath

purstAndPurs :: FilePath -> IO [(FilePath, FilePath)]
purstAndPurs d = do
	map (((d </>) . (<.> "purst") &&& (d </>) . (<.> "purs")) . dropExtension)
		. filter (isExtensionOf "purst") <$> getDirectoryContents d
