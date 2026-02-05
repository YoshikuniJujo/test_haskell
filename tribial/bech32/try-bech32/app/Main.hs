{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Data.ByteString qualified as BS
import Data.Text.IO qualified as T
import System.Environment
import System.Posix.Files
import Lib

main :: IO ()
main = do
	scfp : pbfp : _ <- getArgs
	scbn <- BS.readFile scfp
	pbbn <- BS.readFile pbfp
	print $ someFunc "nsec" scbn
	print $ someFunc "npub" $ BS.tail pbbn
	print $ someFunc "foo" "Hello, world!"

	either (error . show) (T.writeFile "nsec") $ someFunc "nsec" scbn
	setFileMode "nsec" $ ownerReadMode `unionFileModes` ownerWriteMode
	either (error . show) (T.writeFile "npub") $ someFunc "npub" $ BS.tail pbbn
