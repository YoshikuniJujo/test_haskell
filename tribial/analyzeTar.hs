{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

import Numeric
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Exception
import Data.Bits
import Data.Word
import Data.Char
import Data.Time
import Data.Time.Clock.POSIX
import System.IO
import System.IO.Error
import System.Posix
import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Lazy as LBS

type PtrIO = StateT (Ptr Word8) IO

data Header
	= Header {
		name :: FilePath,
		mode :: FileMode,
		uid :: UserID,
		gid :: GroupID,
		size :: FileOffset,
		mtime :: POSIXTime,
		chksum :: Word32,
		typeflag :: TypeFlag,
		linkname :: FilePath,
		magic :: BS.ByteString,
		uname :: String,
		gname :: String,
		devmajor :: BS.ByteString,
		devminor :: BS.ByteString,
		prefix :: FilePath }
	| ZeroHeader
	deriving Show

data TypeFlag
	= RegularFile
	| Link
	| SymLink
	| CharDevice
	| BlockDevice
	| Directory
	| Fifo
	| ContiguousFile
	| UnknownType Word8
	deriving Show

typeFlag :: Word8 -> TypeFlag
typeFlag = \case
	0 -> RegularFile
	0x30 -> RegularFile
	0x31 -> Link
	0x32 -> SymLink
	0x33 -> CharDevice
	0x34 -> BlockDevice
	0x35 -> Directory
	0x36 -> Fifo
	0x37 -> ContiguousFile
	w -> UnknownType w

instance Storable Header where
	sizeOf _ = 512
	alignment _ = 8
	peek p = do
		iz <- isZeroHeader p
		if iz	then return ZeroHeader
			else evalStateT peekHeader $ castPtr p

peekHeader :: PtrIO Header
peekHeader = do
	p <- get
	h <- Header
		<$> (nullTermString <$> readByteString 100)
		<*> (octal <$> readByteString 8)
		<*> (octal <$> readByteString 8)
		<*> (octal <$> readByteString 8)
		<*> (octal <$> readByteString 12)
		<*> (octal <$> readByteString 12)
		<*> (octal <$> readByteString 8)
		<*> (typeFlag . head . BS.unpack <$> readByteString 1)
		<*> (nullTermString <$> readByteString 100)
		<*> readByteString 8
		<*> (nullTermString <$> readByteString 32)
		<*> (nullTermString <$> readByteString 32)
		<*> (BS.takeWhile (/= 0) <$> readByteString 8)
		<*> (BS.takeWhile (/= 0) <$> readByteString 8)
		<*> (nullTermString <$> readByteString 155)
	lift $ do
		pokeArray (p `plusPtr` 148) $ replicate 8 (32 :: Word8)
		cs <- sumBytes 512 p
		when (cs /= chksum h) $ error "not match checksum"
	return h

isZeroHeader :: Ptr Header -> IO Bool
isZeroHeader p = (all (== 0)) <$> peekArray 512 (castPtr p :: Ptr Word8)

octal :: (Num a, Eq a) => BS.ByteString -> a
octal = fst . head . readOct . BSC.unpack

nullTermString :: BS.ByteString -> String
nullTermString = BSC.unpack . BSC.takeWhile (/= '\0')

readByteString :: Word8 -> PtrIO BS.ByteString
readByteString n_ = get >>= \p -> BS.takeWhile (/= 0)
	<$> lift (BSI.create n $ \b -> copyBytes b p n) <* put (p `plusPtr` n)
	where n = fromIntegral n_

sumBytes :: Word16 -> Ptr a -> IO Word32
sumBytes n p = sum . map fromIntegral
	<$> peekArray (fromIntegral n) (castPtr p :: Ptr Word8)

{-

1. make directory
2. set user and group from name
	-> if no name then set user and group from id
3. set mode
4. set modification time

-}

testRun :: FilePath -> IO ()
testRun fp = withDirectory "tmp/" . untar =<< openFile fp ReadMode

untar :: Handle -> IO ()
untar hdl = allocaBytes 512 ((>>) <$> flip (hGetBuf hdl) 512 <*> peek)
	>>= \case
		ZeroHeader -> allocaBytes
				512
				((>>) <$> flip (hGetBuf hdl) 512 <*> peek)
			>>= \case
				ZeroHeader -> return ()
				_ -> error "bad structure"
		hdr -> runHeader hdl hdr >> untar hdl

withDirectory :: FilePath -> IO a -> IO a
withDirectory nd act = getWorkingDirectory >>= \cd ->
	(changeWorkingDirectory nd >> act) `finally` changeWorkingDirectory cd

runHeader :: Handle -> Header -> IO ()
runHeader hdl hdr = case typeflag hdr of
	Directory -> ($ hdr) $ runDirectory
		<$> name
		<*> mode
		<*> ((,) <$> uname <*> uid)
		<*> ((,) <$> gname <*> gid)
		<*> mtime
	RegularFile -> ($ hdr) $ runRegularFile hdl
		<$> name
		<*> mode
		<*> ((,) <$> uname <*> uid)
		<*> ((,) <$> gname <*> gid)
		<*> size
		<*> mtime
	_ -> error "not implemented"

runDirectory ::
	String -> FileMode -> (String, UserID) -> (String, GroupID) ->
	POSIXTime -> IO ()
runDirectory n m (un, ui_) (gn, gi_) mt = do
	createDirectory n m
	ui <- getWithDefault ui_ ((userID <$>) . getUserEntryForName) un
	gi <- getWithDefault gi_ ((groupID <$>) . getGroupEntryForName) gn
	setOwnerAndGroup n ui gi
	setFileTimesHiRes n mt mt

getWithDefault :: b -> (a -> IO b) -> a -> IO b
getWithDefault d lkp k = handleJust
	(\e -> if isDoesNotExistError e then Just d else Nothing)
	return
	(lkp k)

runRegularFile :: Handle ->
	String -> FileMode -> (String, UserID) -> (String, GroupID) ->
	FileOffset -> POSIXTime -> IO ()
runRegularFile hdl n m (un, ui_) (gn, gi_) sz mt = do
	cnt <- readBytes hdl sz
	LBS.writeFile n cnt
	ui <- getWithDefault ui_ ((userID <$>) . getUserEntryForName) un
	gi <- getWithDefault gi_ ((groupID <$>) . getGroupEntryForName) gn
	setOwnerAndGroup n ui gi
	setFileTimesHiRes n mt mt

readBytes :: Handle -> FileOffset -> IO LBS.ByteString
readBytes hdl = (LBS.fromChunks <$>) . rb
	where rb n = BS.hGet hdl 512 >>= \bs -> if n <= 512
		then return [BS.take (fromIntegral n) bs]
		else (bs :) <$> rb (n - 512)
