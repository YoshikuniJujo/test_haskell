{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Foreign.Storable
import Foreign.Marshal
import Foreign.C.Types
import Foreign.C.String
import Control.Exception
import System.Posix.IO
import System.Posix.Files
import System.Posix.Types
import Numeric

import NewIovec
import NewVectoredIo

main :: IO ()
main = main1 >> main2 >> main3 >> main4

main1 :: IO ()
main1 = bracket (createFile "tmp_.txt" fm644) closeFd
	$ \fd -> alloca @Int $ \pint ->
			alloca @CChar $ \pc -> alloca @CChar $ \pc2 -> do
		poke pint 0x3132333435363738
		poke pc $ castCharToCChar 'w'
		poke pc2 $ castCharToCChar '\n'
		print =<< writev fd
			((pint, 1) :-- (pc, 1) :-- (pc2, 1) :-- HtrPtrLenNil)

main2 :: IO ()
main2 = bracket (openFd "tmp_.txt" ReadOnly Nothing defaultFileFlags) closeFd
	$ \fd -> alloca @Int $ \pint ->
			alloca @CChar $ \pc -> alloca @CChar $ \pc2 -> do
		print =<< readv fd
			((pint, 1) :-- (pc, 1) :-- (pc2, 1) :-- HtrPtrLenNil)
		putStrLn . ("0x" ++) . (`showHex` "") =<< peek pint
		print . castCCharToChar =<< peek pc
		print . castCCharToChar =<< peek pc2

main3 :: IO ()
main3 = bracket (createFile "tmp2_.txt" fm644) closeFd
	$ \fd -> withCStringLen "hello\n" $ \cs -> do
		print =<< writev fd (cs :- PtrLenNil)

main4 :: IO ()
main4 = bracket (openFd "tmp2_.txt" ReadOnly Nothing defaultFileFlags) closeFd
	$ \fd -> allocaArray @CChar 6 $ \cs -> do
		print =<< readv fd ((cs, 6) :- PtrLenNil)
		print =<< peekCStringLen (cs, 6)

fm644 :: FileMode
fm644 = foldr1 unionFileModes
	[ownerReadMode, ownerWriteMode, groupReadMode, otherReadMode]
