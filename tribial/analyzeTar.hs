{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Numeric (showOct, readOct)
import Control.Monad (join, when, replicateM_)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, evalStateT, get, put)
import Control.Exception (handleJust, finally)
import Data.Bits ((.&.))
import Data.Bool (bool)
import Data.Word (Word8, Word16, Word32)
import Data.Time.Clock.POSIX (POSIXTime)
import System.Environment (getArgs)
import System.IO (Handle, IOMode(..), openFile, hGetBuf, hPutBuf)
import System.IO.Error (isDoesNotExistError)
import System.Posix (
	DirStream,
	FileStatus, FileMode, FileOffset,
	UserEntry(..), GroupEntry(..), UserID, GroupID,
	getFileStatus,
	isDirectory, isRegularFile,
	fileSize, fileMode, fileOwner, fileGroup,
	modificationTimeHiRes,
	createDirectory, removeDirectory, openDirStream, readDirStream,
	getWorkingDirectory, changeWorkingDirectory,
	removeLink, setFileMode,
	setOwnerAndGroup,
	getUserEntryForName, getGroupEntryForName,
	getUserEntryForID, getGroupEntryForID,
	setFileTimesHiRes )
import System.FilePath
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (Storable(..))
import Foreign.Marshal (
	allocaBytes, peekArray, pokeArray, copyBytes, fillBytes)
import Foreign.ForeignPtr (withForeignPtr)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Lazy as LBS

-- MAIN FUNCTIONS

main :: IO ()
main = mainEx . head =<< getArgs

mainEx :: FilePath -> IO ()
mainEx tfp = do
	putToTmp tfp
	withDirectory "tmp/" $ do
		BS.readFile "tmp/a.txt" >>= BS.putStr
		BS.readFile "tmp/b.txt" >>= BS.putStr
		removeLink "tmp/a.txt"
		removeLink "tmp/b.txt"
		removeDirectory "tmp"

putToTmp :: FilePath -> IO ()
putToTmp fp = withDirectory "tmp/" . untar =<< openFile fp ReadMode

-- UNTAR

untar :: Handle -> IO ()
untar hdl = header hdl >>= \case
	NullHeader -> header hdl >>= \case
		NullHeader -> return ()
		_ -> error "bad structure"
	hdr -> runHeader hdl hdr >> untar hdl

header :: Handle -> IO Header
header hdl = allocaBytes 512 $ (>>) <$> flip (hGetBuf hdl) 512 <*> peek

runHeader :: Handle -> Header -> IO ()
runHeader hdl hdr = case typeflag hdr of
	Directory -> ($ hdr) $ directory
		<$> name <*> mode
		<*> ((,) <$> uname <*> uid)
		<*> ((,) <$> gname <*> gid)
		<*> mtime
	RegularFile -> ($ hdr) $ regularFile hdl
		<$> size
		<*> name <*> mode
		<*> ((,) <$> uname <*> uid)
		<*> ((,) <$> gname <*> gid)
		<*> mtime
	_ -> error "not implemented"

type User = (String, UserID)
type Group = (String, GroupID)

directory :: FilePath -> FileMode -> User -> Group -> POSIXTime -> IO ()
directory n m u g mt = createDirectory n m >> setProperties n u g mt

regularFile :: Handle -> FileOffset ->
	FilePath -> FileMode -> User -> Group -> POSIXTime -> IO ()
regularFile hdl sz n m u g mt = do
	LBS.writeFile n =<< readData hdl sz
	setFileMode n m >> setProperties n u g mt

setProperties :: FilePath -> User -> Group -> POSIXTime -> IO ()
setProperties n (un, ui_) (gn, gi_) mt = do
	join $ setOwnerAndGroup n
		<$> getWithDefault ui_ ((userID <$>) . getUserEntryForName) un
		<*> getWithDefault gi_ ((groupID <$>) . getGroupEntryForName) gn
	setFileTimesHiRes n mt mt

readData :: Handle -> FileOffset -> IO LBS.ByteString
readData hdl = (LBS.fromChunks <$>) . rd
	where rd n = BS.hGet hdl 512 >>= \bs -> if n <= 512
		then return [BS.take (fromIntegral n) bs]
		else (bs :) <$> rd (n - 512)

-- OTHERS

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
	let	(fp, os, l) = BSI.toForeignPtr bs
	withForeignPtr fp $ \b -> do
		copyBytes p (b `plusPtr` os) l
		fillBytes (p `plusPtr` l) 0 (n - l)
	where n = fromIntegral n_

writeString :: Word8 -> String -> PtrIO ()
writeString n = writeBytes n . BSC.pack

writeOctal :: (Integral a, Show a) => Word8 -> a -> PtrIO ()
writeOctal n = writeBytes n . toOctal (n - 1)

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
		flip when (error "not match checksum")
			. (/= 0) . (.&. 0x1ffff)
			. (subtract $ chksum h) =<< sumBytes 512 p
	return h

isNullHeader :: Ptr Header -> IO Bool
isNullHeader p = all (== 0) <$> peekArray 512 (castPtr p :: Ptr Word8)

pokeHeader :: Header -> PtrIO ()
pokeHeader NullHeader = zeroClear 512
pokeHeader h = get >>= \p -> do
	writeString 100 $ name h
	writeOctal 8 $ mode h
	writeOctal 8 $ uid h
	writeOctal 8 $ gid h
	writeOctal 12 $ size h
	writeOctal 12 . truncate $ mtime h
--	writeOctal 8 $ chksum h
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
	lift $ do
		cs <- sumBytes 512 p
		writeOctal 8 cs `evalStateT` (p `plusPtr` 148)

zeroClear :: Word16 -> PtrIO ()
zeroClear n_ = getModify (`plusPtr` n) >>= \p -> lift $ fillBytes p 0 n
	where n = fromIntegral n_

data TypeFlag
	= RegularFile | HardLink | SymLink
	| CharDevice | BlockDevice
	| Directory | Fifo | ContiguousFile
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
	UnknownType w -> w

-- TOOLS

getModify :: Monad m => (s -> s) -> StateT s m s
getModify f = get >>= (>>) <$> put . f <*> return

octal :: (Num a, Eq a) => BS.ByteString -> a
octal = fst . head . readOct . BSC.unpack

toOctal :: (Integral a, Show a) => Word8 -> a -> BS.ByteString
toOctal n x = BSC.pack $ replicate (fromIntegral n - length s) '0' ++ s
	where s = showOct x ""

sumBytes :: Word16 -> Ptr a -> IO Word32
sumBytes n p = sum . map fromIntegral
	<$> peekArray (fromIntegral n) (castPtr p :: Ptr Word8)

withDirectory :: FilePath -> IO a -> IO a
withDirectory nd act = getWorkingDirectory >>= \cd ->
	(changeWorkingDirectory nd >> act) `finally` changeWorkingDirectory cd

getWithDefault :: b -> (a -> IO b) -> a -> IO b
getWithDefault d lkp k = handleJust
	(\e -> if isDoesNotExistError e then Just d else Nothing)
	return
	(lkp k)

-- NEXT
{-

Hard Link
Symbolic Link
FIFO File

Charactor Special File
Block Special File

-}

toByteString :: Ptr a -> Int -> IO BS.ByteString
toByteString p n = BSI.create n $ \b -> copyBytes b (castPtr p) n

-- SAMPLE

sampleDirHeader :: Header
sampleDirHeader = Header {
	name = "hoge/", mode = 0o755, uid = 1000, gid = 1000, size = 0,
	mtime = 1503295499, chksum = 0, typeflag = Directory, linkname = "",
	magic = "ustar  ", uname = "tatsuya", gname = "tatsuya",
	devmajor = "", devminor = "", prefix = "" }

makeSample :: FilePath -> IO ()
makeSample fp = do
	h <- openFile fp WriteMode
	allocaBytes 1536 $ \p -> do
		pokeArray p [sampleDirHeader, NullHeader, NullHeader]
		hPutBuf h p 1536

-- TAR

{-
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
		-}

tar :: FilePath -> [FilePath] -> IO ()
tar tfp (sfp : sfps) = do
	hdl <- openFile tfp WriteMode
	hdrs <- tarGen sfp
	mapM_ (fromHeader hdl) hdrs
	{-
	allocaBytes 512 $ \p -> do
		poke p hdr
		hPutBuf hdl p 512
		-}
	replicateM_ 2 $ allocaBytes 512 $ \p -> do
		poke p NullHeader
		hPutBuf hdl p 512

fromHeader :: Handle -> Header -> IO ()
fromHeader hdl hdr = case typeflag hdr of
	Directory -> allocaBytes 512 $ \p -> do
		poke p hdr
		hPutBuf hdl p 512
	RegularFile -> do
		allocaBytes 512 $ \p -> do
			poke p hdr
			hPutBuf hdl p 512
		sh <- openFile (name hdr) ReadMode
		copyFile sh hdl $ size hdr
	_ -> error "not implemented"

copyFile :: Handle -> Handle -> FileOffset -> IO ()
copyFile src dst sz
	| sz <= 512 = allocaBytes 512 $ \p -> do
		n <- hGetBuf src p 512
		fillBytes (p `plusPtr` n) 0 (512 - n)
		hPutBuf dst p 512
	| otherwise = allocaBytes 512 $ \p -> do
		512 <- hGetBuf src p 512
		hPutBuf dst p 512
		copyFile src dst $ sz - 512

tarGen :: FilePath -> IO [Header]
tarGen fp = do
	fs <- getFileStatus fp
	u <- getUserEntryForID $ fileOwner fs
	g <- getGroupEntryForID $ fileGroup fs
	case getFiletype fs of
		Directory -> do
			putStrLn "Directory"
			(mkDirHeader fp fs u g :) . concat <$>
				(mapDirStream fp tarGen =<< openDirStream fp)
		RegularFile -> do
			putStrLn "Regular File"
			return [mkFileHeader fp fs u g]
		_ -> error "tar: not implemented"

mapDirStream :: FilePath -> (FilePath -> IO a) -> DirStream -> IO [a]
mapDirStream d f ds = do
	fp <- readDirStream ds
	case fp of
		"" -> return []
		'.' : _ -> mapDirStream d f ds
		_ -> (:) <$> f (d </> fp) <*> mapDirStream d f ds

mkDirHeader :: FilePath -> FileStatus -> UserEntry -> GroupEntry -> Header
mkDirHeader fp fs u g = Header {
	name = addTrailingPathSeparator fp,
	mode = fileMode fs,
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
	mode = fileMode fs,
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

getFiletype :: FileStatus -> TypeFlag
getFiletype fs = case (isDirectory fs, isRegularFile fs) of
		(True, False) -> Directory
		(False, True) -> RegularFile
		_ -> error "getFileType: not implemented"
