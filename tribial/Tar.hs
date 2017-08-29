{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Tar (tar, hTar, untar, hUntar) where

import Control.Monad (join, replicateM_, guard)
import Control.Exception (handleJust)
import Data.Time.Clock.POSIX (POSIXTime)
import System.IO (Handle, IOMode(..), withFile, hGetBuf, hPutBuf)
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
import Foreign.Marshal (alloca, allocaBytes, fillBytes)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import Header (
	Header(NullHeader), TypeFlag(..),
	mkDirHeader, mkFileHeader,
	name, mode, uid, gid, size, mtime, typeflag, uname, gname,
	toTypeflag )

-- TAR

tar :: FilePath -> [FilePath] -> IO ()
tar tfp sfps = withFile tfp WriteMode (`hTar` sfps)

hTar :: Handle -> [FilePath] -> IO ()
hTar hdl sfps = do
	ns <- mapM (fromHeader hdl) . concat =<< mapM mkHeaders sfps
	replicateM_ (2 + 19 - (sum ns + 1) `mod` 20)
		$ hPutStorable hdl NullHeader

fromHeader :: Handle -> Header -> IO Int
fromHeader hdl hdr = case typeflag hdr of
	Directory -> 1 <$ hPutStorable hdl hdr
	RegularFile -> do
		hPutStorable hdl hdr
		withFile (name hdr) ReadMode $ \sh ->
			(+ 1) <$> copyBlocks 512 sh hdl (size hdr)
	_ -> error "fromHeader: Not Implemented"

copyBlocks :: Int -> Handle -> Handle -> FileOffset -> IO Int
copyBlocks bs src dst sz
	| sz <= fromIntegral bs = (1 <$) . allocaBytes bs $ \p -> do
		rd <- hGetBuf src p bs
		fillBytes (p `plusPtr` rd) 0 (bs - rd)
		hPutBuf dst p bs
	| otherwise = ((+ 1) <$>) . allocaBytes bs $ \p -> do
		rd <- hGetBuf src p bs
		guard $ rd == bs
		hPutBuf dst p bs
		copyBlocks bs src dst $ sz - fromIntegral bs

mkHeaders :: FilePath -> IO [Header]
mkHeaders fp = do
	fs <- getFileStatus fp
	u <- getUserEntryForID $ fileOwner fs
	g <- getGroupEntryForID $ fileGroup fs
	case toTypeflag fs of
		Directory -> (mkDirHeader fp fs u g :) . concat
			<$> mapDirectory mkHeaders fp
		RegularFile -> return [mkFileHeader fp fs u g]
		_ -> error "mkHeaders: Not Implemented"

-- UNTAR

untar :: FilePath -> IO ()
untar fp = withFile fp ReadMode hUntar

hUntar :: Handle -> IO ()
hUntar hdl = header hdl >>= \case
	NullHeader -> header hdl >>= \case
		NullHeader -> return ()
		_ -> error "hUntar: Bad Structure"
	hdr -> runHeader hdl hdr >> hUntar hdl

header :: Handle -> IO Header
header hdl = alloca $ (>>) <$> flip (hGetBuf hdl) 512 <*> peek

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
directory n m u g mt = resetUpper n
	$ createDirectory n m >> setProperties n u g mt

regularFile :: Handle -> FileOffset ->
	FilePath -> FileMode -> User -> Group -> POSIXTime -> IO ()
regularFile hdl sz n m u g mt = resetUpper n $ do
	LBS.writeFile n =<< readBlocks hdl 512 sz
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
	(\e -> if isDoesNotExistError e then Just d else Nothing)
	return
	(lkp k)

hPutStorable :: Storable s => Handle -> s -> IO ()
hPutStorable h s = alloca $ \p -> poke p s >> hPutBuf h p (sizeOf s)

mapDirectory :: (FilePath -> IO a) -> FilePath -> IO [a]
mapDirectory f d = md =<< openDirStream d
	where md ds = readDirStream ds >>= \case
		"" -> return []
		'.' : _ -> md ds
		fp -> (:) <$> f (d </> fp) <*> md ds

resetUpper :: FilePath -> IO a -> IO a
resetUpper fp act = do
	fs <- getFileStatus d
	act <* setFileTimesHiRes d
		(accessTimeHiRes fs) (modificationTimeHiRes fs)
	where d = takeDirectory $ dropTrailingPathSeparator fp

readBlocks :: Handle -> Int -> FileOffset -> IO LBS.ByteString
readBlocks hdl bs = (LBS.fromChunks <$>) . rd
	where rd n = BS.hGet hdl bs >>= \b -> if n <= fromIntegral bs
		then return [BS.take (fromIntegral n) b]
		else (b :) <$> rd (n - fromIntegral bs)

-- NEXT
{-

Hard Link
Symbolic Link
FIFO File

Charactor Special File
Block Special File

-}
