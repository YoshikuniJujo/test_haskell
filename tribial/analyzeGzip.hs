{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

import Control.Arrow
import Control.Monad
import Data.Bits
import Data.Word
import Data.Time
import Data.Time.Clock.POSIX
import Control.Monad.Trans.State
import System.Environment

import qualified Data.ByteString as BS
import qualified Deflate
import qualified Crc32

main :: IO ()
main = mainEx . head =<< getArgs

mainEx :: FilePath -> IO ()
mainEx fp = BS.putStr =<<
	maybe (error "bad") snd . evalStateT uncompressSt <$> BS.readFile fp

type ByteArrSt = StateT BS.ByteString Maybe

type RawHeaders = (CompMethod, Flags, UTCTime, Word8, OS, Maybe BS.ByteString)

readHeaders :: ByteArrSt RawHeaders
readHeaders = do
	(id1, id2) <- readId
	guard $ id1 == 31 && id2 == 139
	(cm, fs) <- (,)	<$> compMethod <*> flags
	(mt, ef, os) <- (,,) <$> modTime <*> exFlags <*> readOS
	nm <- if fname fs
		then Just <$> nullTerminate
		else return Nothing
	return (cm, fs, mt, ef, os, nm)

uncompressSt :: ByteArrSt (RawHeaders, BS.ByteString)
uncompressSt = do
	hp@(_, p) <- (,) <$> readHeaders <*> StateT Deflate.uncompress
	cs <- getChecksum
	ln <- getPlainLen
	guard $ fromIntegral (BS.length p) == ln
	guard $ Crc32.verify p cs
	return hp

getChecksum :: ByteArrSt Word32
getChecksum = getNumber 4

getPlainLen :: ByteArrSt Word32
getPlainLen = getNumber 4

getByte :: ByteArrSt Word8
getByte = StateT BS.uncons

nullTerminate :: ByteArrSt BS.ByteString
nullTerminate = StateT (Just . BS.break (== 0)) <* getByte

getNumber :: (Bits a, Num a) => Word8 -> ByteArrSt a
getNumber l | l <= 0 = return 0
getNumber l = do
	b <- getByte
	n <- getNumber $ l - 1
	return $ fromIntegral b .|. (n `shiftL` 8)

readId :: ByteArrSt (Word8, Word8)
readId = (,) <$> getByte <*> getByte

data CompMethod
	= Deflate
	| OtherCompMethod Word8
	deriving Show

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)

compMethod :: ByteArrSt CompMethod
compMethod = getByte <&> \case
	8 -> Deflate
	b -> OtherCompMethod b

data Flags = Flags {
	ftext :: Bool,
	fhcrc :: Bool,
	fextra :: Bool,
	fname :: Bool,
	fcomment :: Bool
	} deriving Show

flags :: ByteArrSt Flags
flags = do
	b <- getByte
	guard $ 0xe0 .&. b == 0
	let	[tx, hc, ex, nm, cm] = map (b `testBit`) [0 .. 4]
	return $ Flags {
		ftext = tx,
		fhcrc = hc,
		fextra = ex,
		fname = nm,
		fcomment = cm }

modTime :: ByteArrSt UTCTime
modTime = posixSecondsToUTCTime . fromIntegral
	<$> (getNumber 4 :: ByteArrSt Word32)

exFlags :: ByteArrSt Word8
exFlags = getByte

data OS	= FAT
	| Amiga
	| VMS
	| Unix
	| VM_CMS
	| AtariTos
	| HPFS
	| Machintosh
	| ZSystem
	| CP_M
	| TOPS_20
	| NTFS
	| QDOS
	| AcornRISCOS
	| Unknown
	| UndefinedOS Word8
	deriving Show

word8ToOS :: Word8 -> OS
word8ToOS = \case
	0 -> FAT
	1 -> Amiga
	2 -> VMS
	3 -> Unix
	4 -> VM_CMS
	5 -> AtariTos
	6 -> HPFS
	7 -> Machintosh
	8 -> ZSystem
	9 -> CP_M
	10 -> TOPS_20
	11 -> NTFS
	12 -> QDOS
	13 -> AcornRISCOS
	255 -> Unknown
	w -> UndefinedOS w

readOS :: ByteArrSt OS
readOS = word8ToOS <$> getByte
