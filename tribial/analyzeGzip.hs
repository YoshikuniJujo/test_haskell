{-# LANGUAGE LambdaCase, BinaryLiterals #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

import Control.Arrow
import Control.Monad
import Data.Bits
import Data.Word
import Data.Time
import Data.Time.Clock.POSIX
import Control.Monad.Trans.State

import qualified Data.ByteString as BS
import qualified Deflate

type ByteArrSt = StateT BS.ByteString Maybe

type RawHeaders = ((Word8, Word8), CompMethod, Flags, UTCTime, Word8, OS)

readHeaders :: ByteArrSt RawHeaders
readHeaders = (,,,,,)
	<$> readId
	<*> compMethod
	<*> flags
	<*> modTime
	<*> exFlags
	<*> readOS

notChecked :: ByteArrSt (RawHeaders, BS.ByteString)
notChecked = (,) <$> readHeaders <*> StateT Deflate.uncompress

getByte :: ByteArrSt Word8
getByte = StateT BS.uncons

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
