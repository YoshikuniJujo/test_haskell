{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main, toByteString) where

import Tar

import Control.Exception (finally)
import System.Environment (getArgs)
import System.Posix (
	getWorkingDirectory, changeWorkingDirectory,
	removeDirectory, removeLink )
import Foreign.Ptr
import Foreign.Marshal

import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI

-- MAIN FUNCTIONS

main :: IO ()
main = mainEx . head =<< getArgs

mainEx :: FilePath -> IO ()
mainEx tfp = do
	putToTmp tfp
	withDirectory "tmp/" $ do
		BS.readFile "tmp/a.txt" >>= BS.putStr
		BS.readFile "tmp/b.txt" >>= BS.putStr
		removeLink "tmp/a.txt"
		removeLink "tmp/b.txt"
		removeDirectory "tmp"

putToTmp :: FilePath -> IO ()
putToTmp = withDirectory "tmp/" . untar

withDirectory :: FilePath -> IO a -> IO a
withDirectory nd act = getWorkingDirectory >>= \cd ->
	(changeWorkingDirectory nd >> act) `finally` changeWorkingDirectory cd

toByteString :: Ptr a -> Int -> IO BS.ByteString
toByteString p n = BSI.create n $ \b -> copyBytes b (castPtr p) n
