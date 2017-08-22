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

type PtrIO = StateT (Ptr Word8) IO

data Header = Header {
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
	prefix :: FilePath
	} deriving Show

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
	peek = evalStateT peekHeader . castPtr

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
testRun fp = do
	hdl <- openFile fp ReadMode
	h <- allocaBytes 512 $ \p -> do
		hGetBuf hdl p 512
		peek p
	d <- getWorkingDirectory
	(`finally` changeWorkingDirectory d) $ do
		changeWorkingDirectory "tmp/"
		runHeader h

runHeader :: Header -> IO ()
runHeader h = case typeflag h of
	Directory -> ($ h) $ runDirectory
		<$> name
		<*> mode
		<*> ((,) <$> uname <*> uid)
		<*> ((,) <$> gname <*> gid)
		<*> mtime
	_ -> error "not implemented"

runDirectory ::
	String -> FileMode -> (String, UserID) -> (String, GroupID) ->
	POSIXTime -> IO ()
runDirectory n m (un, ui_) (gn, gi_) mt = do
	createDirectory n m
	ui <- handleJust
		(\e -> if isDoesNotExistError e then Just ui_ else Nothing)
		return
		(userID <$> getUserEntryForName un)
	gi <- handleJust
		(\e -> if isDoesNotExistError e then Just gi_ else Nothing)
		return
		(groupID <$> getGroupEntryForName gn)
	setOwnerAndGroup n ui gi
	setFileTimesHiRes n mt mt
