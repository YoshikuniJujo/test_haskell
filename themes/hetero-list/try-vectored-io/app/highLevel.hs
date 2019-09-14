{-# LANGUAGE GADTs, DataKinds, ScopedTypeVariables, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Foreign.C.Types (CChar)
import Foreign.C.String (castCharToCChar, castCCharToChar)
import System.IO (IOMode(..), withFile)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Numeric (showHex)

import qualified Data.ByteString as BS

import VectoredIo (readVector, writeVector)
import Iovec (ListTuple(..))

tmpDir, tryHlTupleFile, tryHlListFile, tryHlByteFile, tryInadequateReadFile :: FilePath
tmpDir = "tmp"
tryHlTupleFile = tmpDir </> "tryHlTuple.txt"
tryHlListFile = tmpDir </> "tryHlList.txt"
tryHlByteFile = tmpDir </> "tryHlByte.txt"
tryInadequateReadFile = tmpDir </> "tryInadequateReadFile.txt"

main :: IO ()
main = do
	createDirectoryIfMissing False tmpDir
	tryTuple >> tryList >> tryByte >> tryInadequateRead 

tryTuple :: IO ()
tryTuple = do
	withFile tryHlTupleFile WriteMode $ flip writeVector (
		[0x3132333435363738 :: Int] :.
		(castCharToCChar <$> "Hello, world!\n") :. ListTupleNil )
	withFile tryHlTupleFile ReadMode $ \h -> do
		Right (n :. str :. ListTupleNil :: ListTuple [Int, CChar]) <-
			readVector h [1, 14]
		(putStrLn . ("0x" ++) . (`showHex` "")) `mapM_` n
		putStr $ castCCharToChar <$> str

tryList :: IO ()
tryList = do
	withFile tryHlListFile WriteMode $ flip writeVector [
		castCharToCChar <$> "Hello, " :: [CChar],
		castCharToCChar <$> "world!\n" ]
	withFile tryHlListFile ReadMode $ \h -> do
		Right [s1 :: [CChar], s2] <- readVector h [7, 7]
		print $ castCCharToChar <$> s1
		print $ castCCharToChar <$> s2

tryByte :: IO ()
tryByte = do
	withFile tryHlByteFile WriteMode $ flip writeVector 
		["Good-bye, ", "world!\n" :: BS.ByteString]
	withFile tryHlByteFile ReadMode $ \h -> do
		Right [s1 :: BS.ByteString, s2] <- readVector h [10, 7]
		print s1; print s2

tryInadequateRead :: IO ()
tryInadequateRead = do
	withFile tryInadequateReadFile WriteMode $ \h ->
		writeVector h [castCharToCChar <$> "12345678123456" :: [CChar]]
	withFile tryInadequateReadFile ReadMode $ \h -> do
		(Left [s1, s2] :: Either [BS.ByteString] [[Int]]) <-
			readVector h [1, 1]
		print s1; print s2
