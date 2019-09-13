{-# LANGUAGE GADTs, DataKinds, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Foreign.C.Types (CChar)
import Foreign.C.String (castCharToCChar, castCCharToChar)
import System.IO (IOMode(..), withFile)
import System.FilePath ((</>))
import Numeric (showHex)

import qualified Data.ByteString as BS

import VectoredIo (readVector, writeVector)
import Iovec (ListTuple(..))

tmpDir, tryHlTupleFile, tryHlListFile, tryInadequateReadFile :: FilePath
tmpDir = "tmp"
tryHlTupleFile = tmpDir </> "tryHlTuple.txt"
tryHlListFile = tmpDir </> "tryHlList.txt"
tryInadequateReadFile = tmpDir </> "tryInadequateReadFile.txt"

main :: IO ()
main = tryTuple >> tryList >> tryInadequateRead

tryTuple :: IO ()
tryTuple = do
	withFile tryHlTupleFile WriteMode $ \h ->
		writeVector h ([0x3132333435363738 :: Int] :. (castCharToCChar <$> "w\n") :. ListTupleNil)
	withFile tryHlTupleFile ReadMode $ \h -> do
		Right (n :. str :. ListTupleNil :: ListTuple [Int, CChar]) <- readVector h [1, 2]
		mapM_ (putStrLn . ("0x" ++)) $ (`showHex` "") <$> n
		print $ castCCharToChar <$> str

tryList :: IO ()
tryList = do
	withFile tryHlListFile WriteMode $ \h ->
		writeVector h [castCharToCChar <$> "Hello, ", castCharToCChar <$> "world!\n"]
	withFile tryHlListFile ReadMode $ \h -> do
		Right [s1, s2] <- readVector h [7, 7]
		print (castCCharToChar <$> s1 :: String)
		print (castCCharToChar <$> s2 :: String)

tryInadequateRead :: IO ()
tryInadequateRead = do
	withFile tryInadequateReadFile WriteMode $ \h ->
		writeVector h [castCharToCChar <$> "12345678123456"]
	withFile tryInadequateReadFile ReadMode $ \h -> do
		(Left [s1, s2] :: Either [BS.ByteString] [[Int]]) <- readVector h [1, 1]
		print s1
		print s2
