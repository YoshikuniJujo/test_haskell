{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Foreign.Storable (peek, poke)
import Foreign.Marshal (alloca, allocaArray)
import Foreign.C.Types (CChar)
import Foreign.C.String (
	peekCStringLen, withCStringLen, castCCharToChar, castCharToCChar )
import Control.Exception (bracket)
import System.Posix.IO (
	openFd, createFile, OpenMode(..), defaultFileFlags, closeFd )
import System.Posix.Files (
	unionFileModes,
	ownerReadMode, ownerWriteMode, groupReadMode, otherReadMode )
import System.Posix.Types (Fd, FileMode)
import Numeric (showHex)

import Iovec (PtrLenList(..), PtrLenTuple(..))
import VectoredIo (readv, writev)

main :: IO ()
main = main1 >> main2 >> main3 >> main4

withCreateFile :: FilePath -> FileMode -> (Fd -> IO a) -> IO a
withCreateFile fp fm = bracket (createFile fp fm) closeFd

withOpenReadModeFd :: FilePath -> (Fd -> IO a) -> IO a
withOpenReadModeFd fp =
	bracket (openFd fp ReadOnly Nothing defaultFileFlags) closeFd

main1 :: IO ()
main1 = withCreateFile "tmp_.txt" fm644 $ \fd -> alloca @Int $ \pint ->
		alloca @CChar $ \pc -> alloca @CChar $ \pc2 -> do
	poke pint 0x3132333435363738
	poke pc $ castCharToCChar 'w'
	poke pc2 $ castCharToCChar '\n'
	print =<< writev fd
		((pint, 1) :-- (pc, 1) :-- (pc2, 1) :-- PtrLenTupleNil)

main2 :: IO ()
main2 = withOpenReadModeFd "tmp_.txt" $ \fd -> alloca @Int $ \pint ->
		alloca @CChar $ \pc -> alloca @CChar $ \pc2 -> do
	print =<< readv fd
		((pint, 1) :-- (pc, 1) :-- (pc2, 1) :-- PtrLenTupleNil)
	putStrLn . ("0x" ++) . (`showHex` "") =<< peek pint
	print . castCCharToChar =<< peek pc
	print . castCCharToChar =<< peek pc2

main3 :: IO ()
main3 = withCreateFile "tmp2_.txt" fm644 $ \fd ->
	withCStringLen "hello\n" $ \cs ->
		print =<< writev fd (cs :- PtrLenListNil)

main4 :: IO ()
main4 = withOpenReadModeFd "tmp2_.txt" $ \fd -> allocaArray @CChar 6 $ \cs -> do
	print =<< readv fd ((cs, 6) :- PtrLenListNil)
	print =<< peekCStringLen (cs, 6)

fm644 :: FileMode
fm644 = foldr1 unionFileModes
	[ownerReadMode, ownerWriteMode, groupReadMode, otherReadMode]
