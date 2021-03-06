{-# LANGUAGE LambdaCase, OverloadedStrings, BangPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Tar (tar, hTar, untar, hUntar) where

import Control.Monad (join, replicateM_, guard)
import Control.Exception (handleJust)
import Data.Maybe (fromMaybe)
import Data.Bool (bool)
import Data.Time.Clock.POSIX (POSIXTime)
import System.IO (Handle, IOMode(..), withFile, hGetBuf, hPutBuf)
import System.IO.Unsafe (unsafeInterleaveIO)
import System.IO.Error (isDoesNotExistError)
import System.Posix (
	FileMode, FileOffset,
	UserEntry(..), GroupEntry(..), UserID, GroupID,
	getFileStatus,
	fileOwner, fileGroup, accessTimeHiRes, modificationTimeHiRes,
	setFileMode, setOwnerAndGroup, setFileTimesHiRes,
	getUserEntryForName, getGroupEntryForName,
	getUserEntryForID, getGroupEntryForID,
	openDirStream, readDirStream, createDirectory )
import System.FilePath ((</>), takeDirectory, dropTrailingPathSeparator)
import Foreign.Ptr (plusPtr)
import Foreign.Storable (Storable(..))
import Foreign.Marshal (alloca, allocaBytes, fillBytes, copyBytes)
import Foreign.ForeignPtr (withForeignPtr)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSC

import Header (
	Header(NullHeader), TypeFlag(..),
	mkDirHeader, mkFileHeader, mkLongNameHeader,
	name, mode, uid, gid, size, mtime, typeflag, uname, gname,
	toTypeflag )

-- TAR

tar :: FilePath -> [FilePath] -> IO ()
tar tfp sfps = withFile tfp WriteMode (`hTar` sfps)

hTar :: Handle -> [FilePath] -> IO ()
hTar h sfps = do
	ns <- mapM (fromHeader h) . concat =<< mapM mkHeaders sfps
	replicateM_ (2 + 19 - (sum ns + 1) `mod` 20)
		$ hPutStorable h NullHeader

fromHeader :: Handle -> Header -> IO Int
fromHeader hdl hdr = do
	bs <- if (length (name hdr) <= 100) then return 0 else (+ 1) <$> do
		hPutStorable hdl . mkLongNameHeader $ name hdr
		writeBlocks hdl 512 . BSC.pack $ name hdr
	case typeflag hdr of
		Directory -> bs + 1 <$ hPutStorable hdl hdr
		RegularFile -> (+ (bs + 1)) <$> do
			hPutStorable hdl hdr
			withFile (name hdr) ReadMode
				$ \sh -> copyBlocks sh hdl 512 $ size hdr
		tf -> error $ "fromHeader: Not Implemented: " ++ show tf

mkHeaders :: FilePath -> IO [Header]
mkHeaders fp = do
	fs <- getFileStatus fp
	u <- getUserEntryForID $ fileOwner fs
	g <- getGroupEntryForID $ fileGroup fs
	case toTypeflag fs of
		Directory -> (mkDirHeader fp fs u g :) . concat
			<$> mapDirectory mkHeaders fp
		RegularFile -> return [mkFileHeader fp fs u g]
		tf -> error $ "mkHeaders: Not Implemented: " ++ show tf

-- UNTAR

untar :: FilePath -> IO ()
untar fp = withFile fp ReadMode hUntar

hUntar :: Handle -> IO ()
hUntar = hUntarGen Nothing

hUntarGen :: Maybe FilePath -> Handle -> IO ()
hUntarGen mfp hdl = header hdl >>= \case
	NullHeader -> header hdl >>= \case
		NullHeader -> return ()
		_ -> error "hUntar: Bad Structure"
	hdr -> do
		mfp' <- runHeader mfp hdl hdr
		hUntarGen mfp' hdl

header :: Handle -> IO Header
header h = alloca $ (>>) <$> flip (hGetBuf h) 512 <*> peek

runHeader :: Maybe FilePath -> Handle -> Header -> IO (Maybe FilePath)
runHeader mfp hdl hdr = case typeflag hdr of
	Directory -> do
		($ hdr) $ directory nm
			<$> mode
			<*> ((,) <$> uname <*> uid)
			<*> ((,) <$> gname <*> gid)
			<*> mtime
		return Nothing
	RegularFile -> do
		($ hdr) $ regularFile hdl (size hdr) nm
			<$> mode
			<*> ((,) <$> uname <*> uid)
			<*> ((,) <$> gname <*> gid)
			<*> mtime
		return Nothing
	LongName -> do
		!n <- readBlocks hdl 512 (size hdr)
		return . Just . takeWhile (/= '\0') $ LBSC.unpack n
	tf -> error $ "runHeader: not implemented: " ++ show tf
	where nm = fromMaybe (name hdr) mfp

type User = (String, UserID)
type Group = (String, GroupID)

directory :: FilePath -> FileMode -> User -> Group -> POSIXTime -> IO ()
directory n m u g mt = resetUpper n
	$ createDirectory n m >> setProperties n u g mt

regularFile :: Handle -> FileOffset ->
	FilePath -> FileMode -> User -> Group -> POSIXTime -> IO ()
regularFile h sz n m u g mt = resetUpper n $ do
	LBS.writeFile n =<< readBlocks h 512 sz
	setFileMode n m >> setProperties n u g mt

setProperties :: FilePath -> User -> Group -> POSIXTime -> IO ()
setProperties n (un, ui) (gn, gi) mt = do
	join $ setOwnerAndGroup n
		<$> withDefault ui ((userID <$>) . getUserEntryForName) un
		<*> withDefault gi ((groupID <$>) . getGroupEntryForName) gn
	setFileTimesHiRes n mt mt

-- TOOLS

withDefault :: b -> (a -> IO b) -> a -> IO b
withDefault d lkp k = handleJust
	(bool Nothing (Just d) . isDoesNotExistError) return $ lkp k

mapDirectory :: (FilePath -> IO a) -> FilePath -> IO [a]
mapDirectory f d = md =<< openDirStream d
	where md ds = readDirStream ds >>= \case
		"" -> return []
		'.' : _ -> md ds
		fp -> (:) <$> f (d </> fp) <*> md ds

hPutStorable :: Storable s => Handle -> s -> IO ()
hPutStorable h s = alloca $ \p -> poke p s >> hPutBuf h p (sizeOf s)

readBlocks :: Handle -> Int -> FileOffset -> IO LBS.ByteString
readBlocks h bs = (LBS.fromChunks <$>) . rb
	where rb sz = unsafeInterleaveIO $ BS.hGet h bs >>= \b ->
		if sz <= fromIntegral bs
			then return [BS.take (fromIntegral sz) b]
			else (b :) <$> rb (sz - fromIntegral bs)

writeBlocks :: Handle -> Int -> BS.ByteString -> IO Int
writeBlocks dst bs str
	| l <= bs = (1 <$) . allocaBytes bs $ \p ->
		withForeignPtr fp $ \b -> do
			copyBytes p (b `plusPtr` s) l
			fillBytes (p `plusPtr` l) 0 (bs - l)
			hPutBuf dst p bs
	| otherwise = ((+ 1) <$>) . allocaBytes bs $ \p ->
		withForeignPtr fp $ \b -> do
			copyBytes p (b `plusPtr` s) bs
			hPutBuf dst p bs
			str' <- BSI.create (l - bs) $ \nb ->
				copyBytes nb (b `plusPtr` s `plusPtr` bs) (l - bs)
			writeBlocks dst bs str'
	where
	(fp, s, l) = BSI.toForeignPtr str

copyBlocks :: Handle -> Handle -> Int -> FileOffset -> IO Int
copyBlocks src dst bs sz
	| sz <= fromIntegral bs = (1 <$) . allocaBytes bs $ \p -> do
		rd <- hGetBuf src p bs
		fillBytes (p `plusPtr` rd) 0 $ bs - rd
		hPutBuf dst p bs
	| otherwise = ((+ 1) <$>) . allocaBytes bs $ \p -> do
		rd <- hGetBuf src p bs
		guard $ rd == bs
		hPutBuf dst p bs
		copyBlocks src dst bs $ sz - fromIntegral bs

resetUpper :: FilePath -> IO a -> IO a
resetUpper fp act = do
	fs <- getFileStatus d
	act <* setFileTimesHiRes d
		(accessTimeHiRes fs) (modificationTimeHiRes fs)
	where d = takeDirectory $ dropTrailingPathSeparator fp

-- NEXT
{-

Hard Link
Symbolic Link
FIFO File

Charactor Special File
Block Special File

-}
