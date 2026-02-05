{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Data.Text.IO qualified as T
import System.Posix.Files

import Lib

main :: IO ()
main = do
	Right (sc, pb) <- pair'
	T.writeFile "nsec" sc
	setFileMode "nsec" $ ownerReadMode `unionFileModes` ownerWriteMode
	T.writeFile "npub" pb
