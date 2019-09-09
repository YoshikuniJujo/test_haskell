{-# LANGUAGE TypeApplications, DataKinds, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Foreign.Storable (peek, poke)
import Foreign.Marshal (alloca)
import Foreign.C.Types (CChar)
import Foreign.C.String (castCharToCChar, castCCharToChar)
import Control.Exception (bracket)
import System.Posix.IO (
	openFd, createFile, closeFd, OpenMode(..), defaultFileFlags )
import System.Posix.Files (
	unionFileModes,
	ownerReadMode, ownerWriteMode, groupReadMode, otherReadMode )
import System.Posix.Types (FileMode)
import Numeric (showHex)

import HeteroList (HeteroPtrList(..))
import VectoredIo (readv, writev)
import StorableByteString (StorableByteString)

main :: IO ()
main = main1 >> main2

main1 :: IO ()
main1 = do
	bracket (createFile "tmp.txt" fm644) closeFd
		$ \fd -> alloca @Int $ \pint ->
			alloca @CChar $ \pc -> alloca @CChar $ \pc2 -> do 
				poke pint 0x3132333435363738
				poke pc $ castCharToCChar 'w'
				poke pc2 $ castCharToCChar '\n'
				writev fd (pint :-- pc :-- pc2 :-- PtrNil)
					>>= print
	bracket (openFd "tmp.txt" ReadOnly Nothing defaultFileFlags) closeFd
		$ \fd -> alloca @Int $ \pint -> 
			alloca @CChar $ \pc -> alloca @CChar $ \pc2 -> do 
				readv fd (pint :-- pc :-- pc2 :-- PtrNil)
					>>= print
				putStrLn . ("0x" ++) . (`showHex` "") =<< peek pint
				print . castCCharToChar =<< peek pc
				print . castCCharToChar =<< peek pc2

fm644 :: FileMode
fm644 = foldr1 unionFileModes
	[ownerReadMode, ownerWriteMode, groupReadMode, otherReadMode]

main2 :: IO ()
main2 = do
	bracket (createFile "tmp2.txt" fm644) closeFd
		$ \fd -> alloca @(StorableByteString 10) $ \psb -> do
			poke psb "Hello, world!"
			writev fd (psb :-- PtrNil) >>= print
	bracket (openFd "tmp2.txt" ReadOnly Nothing defaultFileFlags) closeFd
		$ \fd -> alloca @(StorableByteString 10) $ \psb -> do
			readv fd (psb :-- PtrNil) >>= print
			print =<< peek psb
