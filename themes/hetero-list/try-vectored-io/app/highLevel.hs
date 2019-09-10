{-# LANGUAGE GADTs, TypeApplications, DataKinds, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Foreign.C.String
import Foreign.C.Types
import Control.Exception
import System.Posix
import Numeric

import VectoredIo
import Iovec

withCreateFile :: FilePath -> FileMode -> (Fd -> IO a) -> IO a
withCreateFile fp fm = bracket (createFile fp fm) closeFd

withOpenReadModeFd :: FilePath -> (Fd -> IO a) -> IO a
withOpenReadModeFd fp =
	bracket (openFd fp ReadOnly Nothing defaultFileFlags) closeFd

main :: IO ()
main = main1 >> main2

main1 :: IO ()
main1 = do
	withCreateFile "tmp_hl.txt" fm644 $ \fd ->
		writeVector fd ([0x3132333435363738 :: Int] :. (castCharToCChar <$> "w\n") :. ListTupleNil)
	withOpenReadModeFd "tmp_hl.txt" $ \fd -> do
--		n :. str :. ListTupleNil <- readVector @(PtrLenTuple [Int, CChar]) fd [1, 2]
		(n :. str :. ListTupleNil :: ListTuple [Int, CChar])  <- readVector fd [1, 2]
--		(n :: [Int]) :. (str :: [CChar]) :. ListTupleNil <- readVector fd [1, 2]
		mapM_ (putStrLn . ("0x" ++)) $ (`showHex` "") <$> n
		print $ castCCharToChar <$> str

main2 :: IO ()
main2 = do
	withCreateFile "tmp_hl2.txt" fm644 $ \fd ->
		writeVector fd [castCharToCChar <$> "Hello, ", castCharToCChar <$> "world!\n"]
	withOpenReadModeFd "tmp_hl2.txt" $ \fd -> do
		[s1, s2] <- readVector fd [7, 7]
		print (castCCharToChar <$> s1 :: String)
		print (castCCharToChar <$> s2 :: String)

fm644 :: FileMode
fm644 = foldr1 unionFileModes
	[ownerReadMode, ownerWriteMode, groupReadMode, otherReadMode]
