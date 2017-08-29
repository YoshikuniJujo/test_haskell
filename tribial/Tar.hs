{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Tar (tar, hTar, untar, hUntar) where

import Control.Monad (join, replicateM_)
import Control.Exception (handleJust)
import Data.Time.Clock.POSIX (POSIXTime)
import System.IO (Handle, IOMode(..), withFile, hGetBuf, hPutBuf)
import System.IO.Error (isDoesNotExistError)
import System.Posix (
	FileStatus, FileMode, FileOffset,
	UserEntry(..), GroupEntry(..), UserID, GroupID,
	DirStream,
	getFileStatus,
	isDirectory, isRegularFile,
	fileOwner, fileGroup, modificationTimeHiRes,
	setFileMode, setOwnerAndGroup, setFileTimesHiRes,
	getUserEntryForName, getGroupEntryForName,
	getUserEntryForID, getGroupEntryForID,
	openDirStream, readDirStream, createDirectory )
import System.FilePath ((</>), takeDirectory, dropTrailingPathSeparator)
import Foreign.Ptr (plusPtr)
import Foreign.Storable (Storable(..))
import Foreign.Marshal (allocaBytes, fillBytes)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import Header (
	Header(NullHeader), TypeFlag(..),
	mkDirHeader, mkFileHeader,
	name, mode, uid, gid, size, mtime, typeflag, uname, gname )

-- TAR

tar :: FilePath -> [FilePath] -> IO ()
tar tfp sfps = withFile tfp WriteMode (`hTar` sfps)

hTar :: Handle -> [FilePath] -> IO ()
hTar hdl sfps = do
	hdrs <- concat <$> mapM tarGen sfps
	ns <- mapM (fromHeader hdl) hdrs
	replicateM_ 2 $ allocaBytes 512 $ \p -> do
		poke p NullHeader
		hPutBuf hdl p 512
	replicateM_ (19 - (sum ns + 1) `mod` 20) $ allocaBytes 512 $ \p -> do
		poke p NullHeader
		hPutBuf hdl p 512

fromHeader :: Handle -> Header -> IO Int
fromHeader hdl hdr = case typeflag hdr of
	Directory -> allocaBytes 512 $ \p -> do
		poke p hdr
		hPutBuf hdl p 512
		return 1
	RegularFile -> do
		allocaBytes 512 $ \p -> do
			poke p hdr
			hPutBuf hdl p 512
		withFile (name hdr) ReadMode $ \sh ->
			(+ 1) <$> copyFile sh hdl (size hdr)
	_ -> error "not implemented"

copyFile :: Handle -> Handle -> FileOffset -> IO Int
copyFile src dst sz
	| sz <= 512 = allocaBytes 512 $ \p -> do
		n <- hGetBuf src p 512
		fillBytes (p `plusPtr` n) 0 (512 - n)
		hPutBuf dst p 512
		return 1
	| otherwise = allocaBytes 512 $ \p -> do
		512 <- hGetBuf src p 512
		hPutBuf dst p 512
		n <- copyFile src dst $ sz - 512
		return $ n + 1

tarGen :: FilePath -> IO [Header]
tarGen fp = do
	fs <- getFileStatus fp
	u <- getUserEntryForID $ fileOwner fs
	g <- getGroupEntryForID $ fileGroup fs
	case getFiletype fs of
		Directory -> (mkDirHeader fp fs u g :) . concat <$>
			(mapDirStream fp tarGen =<< openDirStream fp)
		RegularFile -> return [mkFileHeader fp fs u g]
		_ -> error "tar: not implemented"

mapDirStream :: FilePath -> (FilePath -> IO a) -> DirStream -> IO [a]
mapDirStream d f ds = do
	fp <- readDirStream ds
	case fp of
		"" -> return []
		'.' : _ -> mapDirStream d f ds
		_ -> (:) <$> f (d </> fp) <*> mapDirStream d f ds

getFiletype :: FileStatus -> TypeFlag
getFiletype fs = case (isDirectory fs, isRegularFile fs) of
		(True, False) -> Directory
		(False, True) -> RegularFile
		_ -> error "getFileType: not implemented"

-- UNTAR

untar :: FilePath -> IO ()
untar fp = withFile fp ReadMode hUntar

hUntar :: Handle -> IO ()
hUntar hdl = header hdl >>= \case
	NullHeader -> header hdl >>= \case
		NullHeader -> return ()
		_ -> error "bad structure"
	hdr -> runHeader hdl hdr >> hUntar hdl

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
directory n m u g mt = do
	let	d = takeDirectory $ dropTrailingPathSeparator n
	dmt <- modificationTimeHiRes <$> getFileStatus d
	createDirectory n m >> setProperties n u g mt
	setFileTimesHiRes d dmt dmt

regularFile :: Handle -> FileOffset ->
	FilePath -> FileMode -> User -> Group -> POSIXTime -> IO ()
regularFile hdl sz n m u g mt = do
	let	d = takeDirectory n
	dmt <- modificationTimeHiRes <$> getFileStatus d
	LBS.writeFile n =<< readData hdl sz
	setFileMode n m >> setProperties n u g mt
	setFileTimesHiRes d dmt dmt

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

-- TOOLS

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
