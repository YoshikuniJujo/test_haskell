{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Header (
	Header(NullHeader), TypeFlag(..),
	mkDirHeader, mkFileHeader, mkLongNameHeader, toTypeflag,
	name, mode, uid, gid, size, mtime, typeflag,
	linkname, uname, gname, devmajor, devminor, prefix
	) where

import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, evalStateT, get, put)
import Data.Bits ((.&.))
import Data.Bool (bool)
import Data.Word (Word8, Word16, Word32, Word64)
import Data.Time.Clock.POSIX (POSIXTime)
import System.FilePath (addTrailingPathSeparator)
import System.Posix (
	FileStatus, FileMode, FileOffset,
	UserID, GroupID, UserEntry(..), GroupEntry(..),
	isDirectory, isRegularFile,
	fileSize, fileMode, fileOwner, fileGroup, modificationTimeHiRes )
import Numeric (readOct, showOct)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (Storable(..))
import Foreign.Marshal (peekArray, pokeArray, copyBytes, fillBytes)
import Foreign.ForeignPtr (withForeignPtr)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Internal as BSI

-- HEADER

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

instance Storable Header where
	sizeOf _ = 512
	alignment _ = 8
	peek p = bool (peekHeader `evalStateT` castPtr p) (return NullHeader)
		=<< isNullHeader p
	poke p h = pokeHeader h `evalStateT` castPtr p

peekHeader :: PtrIO Header
peekHeader = get >>= \p -> do
	h <- Header
		<$> readString 100 <*> readOctal 8
		<*> readOctal 8 <*> readOctal 8
		<*> readOctal 12
		<*> readOctal 12
		<*> readOctal 8
		<*> readType
		<*> readString 100
		<*> readBytes 8
		<*> readString 32 <*> readString 32
		<*> readBytes 8 <*> readBytes 8
		<*> readString 155
	lift $ do
		pokeArray (p `plusPtr` 148) $ replicate 8 (32 :: Word8)
		flip when (error "Tar header does not match checksum")
			. (/= 0) . (.&. 0x1ffff)
			. subtract (chksum h) =<< sumBytes 512 p
		when (magic h /= "ustar  ")
			$ error "Magic string shoud be \"ustar  \""
	return h

isNullHeader :: Ptr Header -> IO Bool
isNullHeader p = all (== 0) <$> peekArray 512 (castPtr p :: Ptr Word8)

pokeHeader :: Header -> PtrIO ()
pokeHeader NullHeader = zeroClear 512
pokeHeader h = get >>= \p0 -> do
	writeString 100 $ name h
	writeOctal 8 $ mode h
	writeOctal 8 $ uid h
	writeOctal 8 $ gid h
	writeOctal 12 $ size h
	writeOctal 12 . (truncate :: POSIXTime -> Word64) $ mtime h
	writeString 8 $ replicate 8 ' '
	writeType $ typeflag h
	writeString 100 $ linkname h
	writeBytes 8 $ magic h
	writeString 32 $ uname h
	writeString 32 $ gname h
	writeBytes 8 $ devmajor h
	writeBytes 8 $ devminor h
	writeString 155 $ prefix h
	zeroClear 12
	put $ p0 `plusPtr` 148
	writeOctal 7 =<< lift (sumBytes 512 p0)

mkDirHeader :: FilePath -> FileStatus -> UserEntry -> GroupEntry -> Header
mkDirHeader fp fs u g = Header {
	name = addTrailingPathSeparator fp,
	mode = 0o7777 .&. fileMode fs,
	uid = fileOwner fs,
	gid = fileGroup fs,
	size = 0,
	mtime = modificationTimeHiRes fs,
	chksum = 0,
	typeflag = Directory,
	linkname = "",
	magic = "ustar  ",
	uname = userName u,
	gname = groupName g,
	devmajor = "",
	devminor = "",
	prefix = "" }

mkFileHeader :: FilePath -> FileStatus -> UserEntry -> GroupEntry -> Header
mkFileHeader fp fs u g = Header {
	name = fp,
	mode = 0o7777 .&. fileMode fs,
	uid = fileOwner fs,
	gid = fileGroup fs,
	size = fileSize fs,
	mtime = modificationTimeHiRes fs,
	chksum = 0,
	typeflag = RegularFile,
	linkname = "",
	magic = "ustar  ",
	uname = userName u,
	gname = groupName g,
	devmajor = "",
	devminor = "",
	prefix = "" }

mkLongNameHeader :: FilePath -> Header
mkLongNameHeader fp = Header {
	name = "././@LongLink",
	mode = 0o644,
	uid = 0,
	gid = 0,
	size = fromIntegral $ length fp + 1,
	mtime = 0,
	chksum = 0,
	typeflag = LongName,
	linkname = "",
	magic = "ustar  ",
	uname = "root",
	gname = "root",
	devmajor = "",
	devminor = "",
	prefix = "" }

-- MONAD

type PtrIO = StateT (Ptr Word8) IO

readBytes :: Word8 -> PtrIO BS.ByteString
readBytes n_ = getModify (`plusPtr` n) >>= \p -> lift
	. (BS.takeWhile (/= 0) <$>) . BSI.create n $ \b -> copyBytes b p n
	where n = fromIntegral n_

readString :: Word8 -> PtrIO String
readString = (BSC.unpack <$>) . readBytes

readOctal :: (Num a, Eq a) => Word8 -> PtrIO a
readOctal = (octal <$>) . readBytes

writeBytes :: Word8 -> BS.ByteString -> PtrIO ()
writeBytes n_ bs = getModify (`plusPtr` n) >>= \p -> lift $ do
	withForeignPtr fp $ \b -> do
		copyBytes p (b `plusPtr` s) l
		fillBytes (p `plusPtr` l) 0 (n - l)
	where
	n = fromIntegral n_
	l = min n l_
	(fp, s, l_) = BSI.toForeignPtr bs

writeString :: Word8 -> String -> PtrIO ()
writeString n = writeBytes n . BSC.pack

writeOctal :: (Integral a, Show a) => Word8 -> a -> PtrIO ()
writeOctal n = writeBytes n . toOctal (n - 1)

zeroClear :: Word16 -> PtrIO ()
zeroClear n_ = getModify (`plusPtr` n) >>= \p -> lift $ fillBytes p 0 n
	where n = fromIntegral n_

getModify :: Monad m => (s -> s) -> StateT s m s
getModify f = get >>= (>>) <$> put . f <*> return

-- TYPEFLAG

data TypeFlag
	= RegularFile | HardLink | SymLink
	| CharDevice | BlockDevice
	| Directory | Fifo | ContiguousFile
	| LongName
	| UnknownType Word8
	deriving Show

readType :: PtrIO TypeFlag
readType = typeFlag . head . BS.unpack <$> readBytes 1

typeFlag :: Word8 -> TypeFlag
typeFlag = \case
	0x00 -> RegularFile
	0x30 -> RegularFile
	0x31 -> HardLink
	0x32 -> SymLink
	0x33 -> CharDevice
	0x34 -> BlockDevice
	0x35 -> Directory
	0x36 -> Fifo
	0x37 -> ContiguousFile
	0x4c -> LongName
	w -> UnknownType w

writeType :: TypeFlag -> PtrIO ()
writeType = writeBytes 1 . BS.pack . (: []) . fromTypeFlag

fromTypeFlag :: TypeFlag -> Word8
fromTypeFlag = \case
	RegularFile -> 0x30
	HardLink -> 0x31
	SymLink -> 0x32
	CharDevice -> 0x33
	BlockDevice -> 0x34
	Directory -> 0x35
	Fifo -> 0x36
	ContiguousFile -> 0x37
	LongName -> 0x4c
	UnknownType w -> w

toTypeflag :: FileStatus -> TypeFlag
toTypeflag fs = case (isDirectory fs, isRegularFile fs) of
	(True, False) -> Directory
	(False, True) -> RegularFile
	_ -> error "getFileType: Not Implemented"

-- TOOLS

octal :: (Num a, Eq a) => BS.ByteString -> a
octal bs = case readOct $ BSC.unpack bs of
	(r, _) : _ -> r
	_ -> error $ "octal: can't read: " ++ show bs

toOctal :: (Integral a, Show a) => Word8 -> a -> BS.ByteString
toOctal n x = BSC.pack $ replicate (fromIntegral n - length s) '0' ++ s
	where s = showOct x ""

sumBytes :: Word16 -> Ptr a -> IO Word32
sumBytes n p = sum . map fromIntegral
	<$> peekArray (fromIntegral n) (castPtr p :: Ptr Word8)
