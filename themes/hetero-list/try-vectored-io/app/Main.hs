{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Foreign.Marshal (allocaArray, peekArray, pokeArray)
import Foreign.C.Types (CChar)
import Foreign.C.String (
	peekCStringLen, withCStringLen, castCCharToChar, castCharToCChar )
import Control.Exception (bracket)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.Posix.IO (
	openFd, createFile, OpenMode(..), defaultFileFlags, closeFd )
import System.Posix.Files (
	unionFileModes,
	ownerReadMode, ownerWriteMode, groupReadMode, otherReadMode )
import System.Posix.Types (Fd, FileMode)
import Numeric (showHex)

import Iovec (ArrayList(..), ArrayTuple(..))
import VectoredIo (readv, writev)

tmpDir, tryTupleFile, tryListFile :: FilePath
tmpDir = "tmp"
tryTupleFile = tmpDir </> "tryTuple.txt"
tryListFile = tmpDir </> "tryList.txt"

main :: IO ()
main = do
	createDirectoryIfMissing False tmpDir
	tryTuple >> tryList

tryTuple :: IO ()
tryTuple = do
	withCreateFile tryTupleFile fm644 $ \fd ->
			allocaArray 2 $ \pint -> allocaArray 2 $ \pc -> do
		pokeArray @Int pint [0x3132333435363738, 0x3938373635343332]
		pokeArray pc $ castCharToCChar <$> "w\n"
		print =<< writev fd ((pint, 2) :-- (pc, 2) :-- ArrayTupleNil)
	withOpenReadModeFd tryTupleFile $ \fd ->
			allocaArray 2 $ \pint -> allocaArray 2 $ \pc -> do
		print =<< readv fd ((pint, 2) :-- (pc, 2) :-- ArrayTupleNil)
		mapM_ putStrLn . (("0x" ++) . (`showHex` "") <$>)
			=<< peekArray @Int 2 pint
		print . (castCCharToChar <$>) =<< peekArray 2 pc

tryList :: IO ()
tryList = do
	withCreateFile tryListFile fm644 $ \fd ->
		withCStringLen "hello\n" $ \s1 ->
			withCStringLen "world\n" $ \s2 ->
				print =<< writev fd (s1 :- s2 :- ArrayListNil)
	withOpenReadModeFd tryListFile $ \fd ->
		allocaArray @CChar 6 $ \s1 -> allocaArray 6 $ \s2 -> do
			print =<< readv fd ((s1, 6) :- (s2, 6) :- ArrayListNil)
			print =<< peekCStringLen (s1, 6)
			print =<< peekCStringLen (s2, 6)

withCreateFile :: FilePath -> FileMode -> (Fd -> IO a) -> IO a
withCreateFile fp fm = bracket (createFile fp fm) closeFd

withOpenReadModeFd :: FilePath -> (Fd -> IO a) -> IO a
withOpenReadModeFd fp =
	bracket (openFd fp ReadOnly Nothing defaultFileFlags) closeFd

fm644 :: FileMode
fm644 = foldr1 unionFileModes
	[ownerReadMode, ownerWriteMode, groupReadMode, otherReadMode]
