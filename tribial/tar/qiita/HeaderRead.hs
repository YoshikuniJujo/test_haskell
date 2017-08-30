{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Data.Word
import Data.Time.Clock.POSIX
import System.Posix

import qualified Data.ByteString as BS

data Header
	= Header {
		name :: FilePath, mode :: FileMode,
		uid :: UserID, gid :: GroupID,
		size :: FileOffset,
		mtime :: POSIXTime,
		chksum :: Word32,
		typeflag :: TypeFlag,
		linkname :: FilePath,
		magic :: BS.ByteString,
		uname :: String, gname :: String,
		devmajor :: BS.ByteString, devminor :: BS.ByteString,
		prefix :: FilePath }
	| NullHeader
	deriving Show

data TypeFlag
	= RegularFile | HardLink | SymLink
	| CharDevice | BlockDevice
	| Directory | Fifo | ContiguousFile
	| UnknownType Word8
	deriving Show
