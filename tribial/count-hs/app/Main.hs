{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Data.List
import Data.Char
import System.Directory
import System.FilePath

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

main :: IO ()
main = putStrLn "Slozsoft"

getDirectoryContents' :: FilePath -> IO [FilePath]
getDirectoryContents' dr = map (dr </>) . filter (not . ("." `isPrefixOf`)) <$> getDirectoryContents dr

getHs :: FilePath -> IO [FilePath]
getHs dr = filterM doesFileExist . filter (".hs" `isExtensionOf`) =<< getDirectoryContents' dr

getDir :: FilePath -> IO [FilePath]
getDir dr = filterM doesDirectoryExist =<< getDirectoryContents' dr

countLines :: FilePath -> IO Int
countLines fp = length . filter (BSC.any $ not . isSpace) . BSC.lines <$> BS.readFile fp

countHsLines :: FilePath -> IO Int
countHsLines fp = do
	hs <- getHs fp
	ls <- sum <$> countLines `mapM` hs
	ds <- getDir fp
	ls' <- sum <$> countHsLines `mapM` ds
	return $ ls + ls'
