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
import Data.Foldable
import Data.Bits
import Data.Maybe
import Data.Sequence qualified as Seq
import Data.Sequence.ToolsYj qualified as Seq
import Data.Sequence.Word8 qualified as Seq
import Data.Word
import Data.Char
import Data.ByteString qualified as BS
import Data.ByteString.ToolsYj qualified as BS

import Control.Monad.Yaftee.Pipe.Sequence.Crc32 qualified as PipeCrc32

import Data.Word.Crc32 qualified as Crc32

ids0 :: Seq.Seq Word8
ids0 = Seq.fromList $ fromIntegral . ord <$> "\x1f\x8b"

data GzipHeaderRaw = GzipHeaderRaw {
	gzipHeaderRawCompressionMethod :: CompressionMethod,
	gzipHeaderRawFlags :: FlagsRaw,
	gzipHeaderRawModificationTime :: CTime,
	gzipHeaderRawExtraFlags :: Word8,
	gzipHeaderRawOperatingSystem :: OS,
	gzipHeaderRawExtraField :: [ExtraField],
	gzipHeaderRawFileName :: Maybe (Seq.Seq Word8),
	gzipHeaderRawComment :: Maybe (Seq.Seq Word8)
	}
	deriving Show

data GzipHeader = GzipHeader {
	gzipHeaderCompressionMethod :: CompressionMethod,
	gzipHeaderFlags :: Flags,
	gzipHeaderModificationTime :: CTime, -- Word32,
	gzipHeaderExtraFlags :: Word8,
	gzipHeaderOperatingSystem :: OS,
	gzipHeaderExtraField :: [ExtraField],
	gzipHeaderFileName :: Maybe (Seq.Seq Word8),
	gzipHeaderComment :: Maybe (Seq.Seq Word8) }
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

encodeGzipHeader :: GzipHeader -> Seq.Seq Word8
encodeGzipHeader hdr_ = let
	rslt = ids0 Seq.><
		((unCompressionMethod (gzipHeaderRawCompressionMethod hdr) `Seq.cons`
			(encodeFlags (gzipHeaderRawFlags hdr) `Seq.cons`
			Seq.fromBits' (cTimeToWord32 $ gzipHeaderRawModificationTime hdr))) Seq.><
		(gzipHeaderRawExtraFlags hdr `Seq.cons`
			(unOS (gzipHeaderRawOperatingSystem hdr) `Seq.cons` Seq.Empty)) Seq.><
			(if (null efs) then Seq.Empty else (word16ToBs lnefs Seq.>< efsbs)) Seq.><
			maybe Seq.Empty (`Seq.snoc` 0) (gzipHeaderRawFileName hdr) Seq.><
			maybe Seq.Empty (`Seq.snoc` 0) (gzipHeaderRawComment hdr)) in
	rslt <> if (flagsRawHcrc $ gzipHeaderRawFlags hdr) then crc16 rslt else Seq.Empty
	where
	hdr = gzipHeaderToRaw hdr_
	efs = gzipHeaderRawExtraField hdr
	efsbs = encodeExtraFields efs
	lnefs = fromIntegral $ length efsbs

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
	gzipHeaderFileName = Just . Seq.fromList $ fromIntegral . ord <$> "abcd.txt",
	gzipHeaderComment = Nothing }

crc' :: Seq.Seq Word8 -> Seq.Seq Word8
crc' = Seq.fromBits' . crc

crc16 :: Seq.Seq Word8 -> Seq.Seq Word8
crc16 = Seq.take 2 . crc'

data ExtraField = ExtraField {
	extraFieldSi1 :: Word8,
	extraFieldSi2 :: Word8,
	extraFieldData :: Seq.Seq Word8 }
	deriving Show

encodeExtraFields :: [ExtraField] -> Seq.Seq Word8
encodeExtraFields = mconcat . (encodeExtraField <$>)

encodeExtraField :: ExtraField -> Seq.Seq Word8
encodeExtraField ExtraField {
	extraFieldSi1 = si1,
	extraFieldSi2 = si2,
	extraFieldData = dt } =
		Seq.fromList [si1, si2] Seq.><
		word16ToBs (fromIntegral $ length dt) Seq.>< dt

decodeExtraFields :: Seq.Seq Word8 -> [ExtraField]
decodeExtraFields Seq.Empty = []
decodeExtraFields bs = let
	(ef, bs') = decodeExtraField bs in
	ef : decodeExtraFields bs'

decodeExtraField :: Seq.Seq Word8 -> (ExtraField, Seq.Seq Word8)
decodeExtraField bs = case toList `first` Seq.splitAt 2 bs of
	([si1, si2], bs') -> let
		(ln, bs'') = (fromIntegral . bsToWord16) `first` Seq.splitAt 2 bs'
		(dt, bs''') = Seq.splitAt ln bs'' in (
		ExtraField {
			extraFieldSi1 = si1,
			extraFieldSi2 = si2,
			extraFieldData = dt },
		bs''' )
	_ -> error "bad"

word16ToBs :: Word16 -> Seq.Seq Word8
word16ToBs w = Seq.fromList $ fromIntegral <$> [w .&. 0xff, w `shiftR` 8]

bsToWord16 :: Seq.Seq Word8 -> Word16
bsToWord16 bs = case fromIntegral <$> toList bs of
	[w0, w1] -> w0 .|. w1 `shiftL` 8
	_ -> error "bad"

crc :: Seq.Seq Word8 -> Crc32.C
crc = complement . PipeCrc32.step Crc32.initial
