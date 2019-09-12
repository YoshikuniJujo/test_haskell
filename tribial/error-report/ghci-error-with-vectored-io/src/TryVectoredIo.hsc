{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryVectoredIo (tryWritev, tryReadv) where

import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(..))
import Foreign.Marshal (alloca, allocaBytes)
import Foreign.C.Types (CInt(..), CChar, CSize(..))
import Foreign.C.String (peekCStringLen, withCStringLen)
import Control.Exception (bracket)
import Data.Int (Int64)
import System.Posix (
	Fd(..), FileMode, openFd, createFile, closeFd,
	OpenMode(..), defaultFileFlags,
	unionFileModes,
	ownerReadMode, ownerWriteMode, groupReadMode, otherReadMode )

#include <sys/uio.h>

foreign import ccall "readv"
	c_readv :: Fd -> Ptr Iovec -> CInt -> IO #type ssize_t

tryReadv :: IO ()
tryReadv = withOpenReadModeFd "foo.txt" $ \fd ->
	allocaBytes 5 $ \pc -> alloca $ \piov -> do
		poke piov $ Iovec (pc, 5)
		print =<< c_readv fd piov 1
		print =<< peekCStringLen (pc, 5)

withOpenReadModeFd :: FilePath -> (Fd -> IO a) -> IO a
withOpenReadModeFd fp =
	bracket (openFd fp ReadOnly Nothing defaultFileFlags) closeFd

tryWritev :: IO ()
tryWritev = withCreateFile "foo.txt" fm644 $ \fd ->
	withCStringLen "hello" $ \(pc, ln) -> alloca $ \piov -> do
		poke piov $ Iovec (pc, fromIntegral ln)
		print =<< c_writev fd piov 1

foreign import ccall "writev"
	c_writev :: Fd -> Ptr Iovec -> CInt -> IO #type ssize_t

newtype {-# CTYPE "sys/uio.h" "struct iovec" #-} Iovec = Iovec (Ptr CChar, CSize) deriving Show

instance Storable Iovec where
	sizeOf _ = #size struct iovec
	alignment _ = #alignment struct iovec
	peek p = Iovec <$> ((,) <$> #{peek struct iovec, iov_base} p <*> #{peek struct iovec, iov_len} p)
	poke p (Iovec (pc, n)) = #{poke struct iovec, iov_base} p pc >> #{poke struct iovec, iov_len} p n

withCreateFile :: FilePath -> FileMode -> (Fd -> IO a) -> IO a
withCreateFile fp fm = bracket (createFile fp fm) closeFd

fm644 :: FileMode
fm644 = foldr1 unionFileModes
	[ownerReadMode, ownerWriteMode, groupReadMode, otherReadMode]
