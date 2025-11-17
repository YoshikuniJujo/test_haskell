{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Gzip.Header (

	-- * GZIP HEADER

	GzipHeader(..),
	CompressionMethod(..), Flags(..), OS(..), ExtraField(..),

	-- * ENCODE

	encodeGzipHeader,

	-- * DECODE

	FlagsRaw(..), readFlags,
	decodeExtraFields,

	-- * SAMPLE

	sampleGzipHeader,

	) where

import Foreign.C.Types
import Control.Arrow
import Data.Bits
import Data.Maybe
import Data.Word
import Data.ByteString qualified as BS
import Data.ByteString.ToolsYj qualified as BS

import Control.Monad.Yaftee.Pipe.ByteString.Crc qualified as PipeCrc

ids0 :: BS.ByteString
ids0 = "\x1f\x8b"

data GzipHeaderRaw = GzipHeaderRaw {
	gzipHeaderRawCompressionMethod :: CompressionMethod,
	gzipHeaderRawFlags :: FlagsRaw,
	gzipHeaderRawModificationTime :: CTime,
	gzipHeaderRawExtraFlags :: Word8,
	gzipHeaderRawOperatingSystem :: OS,
	gzipHeaderRawExtraField :: [ExtraField],
	gzipHeaderRawFileName :: Maybe BS.ByteString,
	gzipHeaderRawComment :: Maybe BS.ByteString
	}
	deriving Show

data GzipHeader = GzipHeader {
	gzipHeaderCompressionMethod :: CompressionMethod,
	gzipHeaderFlags :: Flags,
	gzipHeaderModificationTime :: CTime, -- Word32,
	gzipHeaderExtraFlags :: Word8,
	gzipHeaderOperatingSystem :: OS,
	gzipHeaderExtraField :: [ExtraField],
	gzipHeaderFileName :: Maybe BS.ByteString,
	gzipHeaderComment :: Maybe BS.ByteString }
	deriving Show

gzipHeaderToRaw :: GzipHeader -> GzipHeaderRaw
gzipHeaderToRaw GzipHeader {
	gzipHeaderCompressionMethod = cm,
	gzipHeaderFlags = fs,
	gzipHeaderModificationTime = ct,
	gzipHeaderExtraFlags = efs,
	gzipHeaderOperatingSystem = os,
	gzipHeaderExtraField = eflds,
	gzipHeaderFileName = mfn,
	gzipHeaderComment = mcmmt } = GzipHeaderRaw {
	gzipHeaderRawCompressionMethod = cm,
	gzipHeaderRawFlags =
		flagsToRaw (isJust mfn) (isJust mcmmt) (not $ null eflds) fs,
	gzipHeaderRawModificationTime = ct,
	gzipHeaderRawExtraFlags = efs,
	gzipHeaderRawOperatingSystem = os,
	gzipHeaderRawExtraField = eflds,
	gzipHeaderRawFileName = mfn,
	gzipHeaderRawComment = mcmmt }

cTimeToWord32 :: CTime -> Word32
cTimeToWord32 (CTime w) = fromIntegral w

newtype CompressionMethod = CompressionMethod {
	unCompressionMethod :: Word8 }

pattern CompressionMethodDeflate :: CompressionMethod
pattern CompressionMethodDeflate = CompressionMethod 8

instance Show CompressionMethod where
	show CompressionMethodDeflate = "CompressionMethodDeflate"
	show cm = "(CompressionMethod " ++ show cm ++ ")"

newtype OS = OS { unOS :: Word8 }

pattern OSUnix :: OS
pattern OSUnix = OS 3

instance Show OS where
	show OSUnix = "OSUnix"
	show os = "(OS " ++ show os ++ ")"

readFlags :: Word8 -> Maybe FlagsRaw
readFlags w = if or $ (w `testBit`) <$> [5 .. 7]
	then Nothing
	else Just FlagsRaw {
		flagsRawText = w `testBit` 0,
		flagsRawHcrc = w `testBit` 1,
		flagsRawExtra = w `testBit` 2,
		flagsRawName = w `testBit` 3,
		flagsRawComment = w `testBit` 4 }

encodeFlags :: FlagsRaw -> Word8
encodeFlags fs = foldl setBit 0 xs
	where
	xs = map fst . filter snd . zip [0 ..] $ ($ fs) <$>
		[flagsRawText, flagsRawHcrc, flagsRawExtra, flagsRawName, flagsRawComment]

data FlagsRaw = FlagsRaw {
	flagsRawText :: Bool,
	flagsRawHcrc :: Bool,
	flagsRawExtra :: Bool,
	flagsRawName :: Bool,
	flagsRawComment :: Bool }
	deriving Show

data Flags = Flags {
	flagsText :: Bool,
	flagsHcrc :: Bool }
	deriving Show

flagsToRaw :: Bool -> Bool -> Bool -> Flags -> FlagsRaw
flagsToRaw fn cmmt eflds Flags {
	flagsText = t,
	flagsHcrc = h } = FlagsRaw {
	flagsRawText = t,
	flagsRawHcrc = h,
	flagsRawExtra = eflds,
	flagsRawName = fn,
	flagsRawComment = cmmt }

encodeGzipHeader :: GzipHeader -> BS.ByteString
encodeGzipHeader hdr_ = let
	rslt = ids0 <>
		((unCompressionMethod (gzipHeaderRawCompressionMethod hdr) `BS.cons`
			(encodeFlags (gzipHeaderRawFlags hdr) `BS.cons`
			BS.fromBits' (cTimeToWord32 $ gzipHeaderRawModificationTime hdr))) <>
		(gzipHeaderRawExtraFlags hdr `BS.cons`
			(unOS (gzipHeaderRawOperatingSystem hdr) `BS.cons` BS.empty)) <>
			(if (null efs) then BS.empty else (word16ToBs lnefs <> efsbs)) <>
			maybe BS.empty (`BS.snoc` 0) (gzipHeaderRawFileName hdr) <>
			maybe BS.empty (`BS.snoc` 0) (gzipHeaderRawComment hdr)) in
	rslt <> if (flagsRawHcrc $ gzipHeaderRawFlags hdr) then crc16 rslt else BS.empty
	where
	hdr = gzipHeaderToRaw hdr_
	efs = gzipHeaderRawExtraField hdr
	efsbs = encodeExtraFields efs
	lnefs = fromIntegral $ BS.length efsbs

sampleGzipHeader :: GzipHeader
sampleGzipHeader = GzipHeader {
	gzipHeaderCompressionMethod = CompressionMethodDeflate,
	gzipHeaderFlags = Flags {
		flagsText = False,
		flagsHcrc = False },
	gzipHeaderModificationTime = 1743055415,
	gzipHeaderExtraFlags = 0,
	gzipHeaderOperatingSystem = OSUnix,
	gzipHeaderExtraField = [],
	gzipHeaderFileName = Just "abcd.txt",
	gzipHeaderComment = Nothing }

crc' :: BS.ByteString -> BS.ByteString
crc' = BS.fromBits' . crc

crc16 :: BS.ByteString -> BS.ByteString
crc16 = BS.take 2 . crc'

data ExtraField = ExtraField {
	extraFieldSi1 :: Word8,
	extraFieldSi2 :: Word8,
	extraFieldData :: BS.ByteString }
	deriving Show

encodeExtraFields :: [ExtraField] -> BS.ByteString
encodeExtraFields = mconcat . (encodeExtraField <$>)

encodeExtraField :: ExtraField -> BS.ByteString
encodeExtraField ExtraField {
	extraFieldSi1 = si1,
	extraFieldSi2 = si2,
	extraFieldData = dt } =
		BS.pack [si1, si2] <>
		word16ToBs (fromIntegral $ BS.length dt) <> dt

decodeExtraFields :: BS.ByteString -> [ExtraField]
decodeExtraFields "" = []
decodeExtraFields bs = let
	(ef, bs') = decodeExtraField bs in
	ef : decodeExtraFields bs'

decodeExtraField :: BS.ByteString -> (ExtraField, BS.ByteString)
decodeExtraField bs = case BS.unpack `first` BS.splitAt 2 bs of
	([si1, si2], bs') -> let
		(ln, bs'') = (fromIntegral . bsToWord16) `first` BS.splitAt 2 bs'
		(dt, bs''') = BS.splitAt ln bs'' in (
		ExtraField {
			extraFieldSi1 = si1,
			extraFieldSi2 = si2,
			extraFieldData = dt },
		bs''' )
	_ -> error "bad"

word16ToBs :: Word16 -> BS.ByteString
word16ToBs w = BS.pack $ fromIntegral <$> [w .&. 0xff, w `shiftR` 8]

bsToWord16 :: BS.ByteString -> Word16
bsToWord16 bs = case fromIntegral <$> BS.unpack bs of
	[w0, w1] -> w0 .|. w1 `shiftL` 8
	_ -> error "bad"

crc :: BS.ByteString -> Word32
crc = complement . PipeCrc.crc32StepBS 0xffffffff
