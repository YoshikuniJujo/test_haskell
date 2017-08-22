{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Numeric (readOct)
import Control.Monad (join, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, evalStateT, get, put)
import Control.Exception (handleJust, finally)
import Data.Bool (bool)
import Data.Word (Word8, Word16, Word32)
import Data.Time.Clock.POSIX (POSIXTime)
import System.Environment (getArgs)
import System.IO (Handle, IOMode(..), openFile, hGetBuf)
import System.IO.Error (isDoesNotExistError)
import System.Posix (
	FileMode, FileOffset,
	UserEntry(..), GroupEntry(..), UserID, GroupID,
	createDirectory, removeDirectory,
	getWorkingDirectory, changeWorkingDirectory,
	removeLink, setFileMode,
	setOwnerAndGroup, getUserEntryForName, getGroupEntryForName,
	setFileTimesHiRes )
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (Storable(..))
import Foreign.Marshal (allocaBytes, peekArray, pokeArray, copyBytes)

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

data Header
	= Header {
		name :: FilePath, mode :: FileMode,
		uid :: UserID, gid :: GroupID,
		size :: FileOffset,
		mtime :: POSIXTime,
		chksum :: Word32,
		typeflag :: TypeFlag,
		_linkname :: FilePath,
		_magic :: BS.ByteString,
		uname :: String, gname :: String,
		_devmajor :: BS.ByteString, _devminor :: BS.ByteString,
		_prefix :: FilePath }
	| NullHeader
	deriving Show

instance Storable Header where
	sizeOf _ = 512
	alignment _ = 8
	peek p = isNullHeader p
		>>= bool (peekHeader `evalStateT` castPtr p) (return NullHeader)
	poke = error "not implemented"

peekHeader :: PtrIO Header
peekHeader = do
	p <- get
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
		cs <- sumBytes 512 p
		when (cs /= chksum h) $ error "not match checksum"
	return h

isNullHeader :: Ptr Header -> IO Bool
isNullHeader p = all (== 0) <$> peekArray 512 (castPtr p :: Ptr Word8)

data TypeFlag
	= RegularFile | Link | SymLink | CharDevice
	| BlockDevice | Directory | Fifo | ContiguousFile
	| UnknownType Word8
	deriving Show

readType :: PtrIO TypeFlag
readType = typeFlag . head . BS.unpack <$> readBytes 1

typeFlag :: Word8 -> TypeFlag
typeFlag = \case
	0x00 -> RegularFile
	0x30 -> RegularFile
	0x31 -> Link
	0x32 -> SymLink
	0x33 -> CharDevice
	0x34 -> BlockDevice
	0x35 -> Directory
	0x36 -> Fifo
	0x37 -> ContiguousFile
	w -> UnknownType w

untar :: Handle -> IO ()
untar hdl = allocaBytes 512 ((>>) <$> flip (hGetBuf hdl) 512 <*> peek)
	>>= \case
		NullHeader -> allocaBytes
				512
				((>>) <$> flip (hGetBuf hdl) 512 <*> peek)
			>>= \case
				NullHeader -> return ()
				_ -> error "bad structure"
		hdr -> runHeader hdl hdr >> untar hdl

runHeader :: Handle -> Header -> IO ()
runHeader hdl hdr = case typeflag hdr of
	Directory -> ($ hdr) $ directory
		<$> name
		<*> mode
		<*> ((,) <$> uname <*> uid)
		<*> ((,) <$> gname <*> gid)
		<*> mtime
	RegularFile -> ($ hdr) $ regularFile hdl
		<$> name
		<*> mode
		<*> ((,) <$> uname <*> uid)
		<*> ((,) <$> gname <*> gid)
		<*> size
		<*> mtime
	_ -> error "not implemented"

directory :: FilePath -> FileMode ->
	(String, UserID) -> (String, GroupID) ->
	POSIXTime -> IO ()
directory n m u g mt = do
	createDirectory n m
	setProperties n u g mt

regularFile :: Handle -> String -> FileMode ->
	(String, UserID) -> (String, GroupID) ->
	FileOffset -> POSIXTime -> IO ()
regularFile hdl n m u g sz mt = do
	cnt <- readData hdl sz
	LBS.writeFile n cnt
	setFileMode n m
	setProperties n u g mt

setProperties ::
	FilePath -> (String, UserID) -> (String, GroupID) -> POSIXTime -> IO ()
setProperties n (un, ui_) (gn, gi_) mt = do
	join $ setOwnerAndGroup n
		<$> getWithDefault ui_ ((userID <$>) . getUserEntryForName) un
		<*> getWithDefault gi_ ((groupID <$>) . getGroupEntryForName) gn
	setFileTimesHiRes n mt mt

readData :: Handle -> FileOffset -> IO LBS.ByteString
readData hdl = (LBS.fromChunks <$>) . rb
	where rb n = BS.hGet hdl 512 >>= \bs -> if n <= 512
		then return [BS.take (fromIntegral n) bs]
		else (bs :) <$> rb (n - 512)

-- TOOLS

getModify :: Monad m => (s -> s) -> StateT s m s
getModify f = get >>= (>>) <$> put . f <*> return

octal :: (Num a, Eq a) => BS.ByteString -> a
octal = fst . head . readOct . BSC.unpack

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
