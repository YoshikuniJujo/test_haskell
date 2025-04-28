{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Yaftee.UseFTCQ.Pipe.Gzip where

import Foreign.C.Types
import Control.Arrow
import Control.Monad
import Data.Bits
import Data.Word
import Data.ByteString qualified as BS
import Data.BitArray qualified as BitArray

import Yaftee.UseFTCQ.Eff qualified as Eff
import Yaftee.UseFTCQ.Pipe qualified as Pipe
import Yaftee.UseFTCQ.State qualified as State
import Yaftee.UseFTCQ.Except qualified as Except
import Yaftee.UseFTCQ.Fail qualified as Fail
import Yaftee.OpenUnion qualified as Union

import Yaftee.UseFTCQ.HFreer qualified as HFreer

import Yaftee.UseFTCQ.Pipe.ByteString.OnDemand
import Yaftee.UseFTCQ.Pipe.Crc

import Tools.ByteStringNum

readMagic :: (
	Union.Member Pipe.P effs,
	Union.Member (State.S Request) effs ) =>
	Eff.E effs BS.ByteString o BS.ByteString
readMagic = do
	State.put $ RequestBytes 2
	Pipe.await

readMagic' :: (
	Union.Member Pipe.P effs,
	Union.Member (State.S Request) effs,
	Union.Member (State.S Crc) effs,
	Union.Member (Except.E String) effs,
	Union.Member Fail.F effs ) =>
	Eff.E effs BS.ByteString BS.ByteString ()
readMagic' = do
--	crcPipe Pipe.=$= do
--	foobar Pipe.=$= do
		State.put $ RequestBytes 2
		ids <- Pipe.await
--		when (ids /= "\31\139") do
--			error "barbaz"
--			Except.throw @String "Bad magic"
		Pipe.yield ids
--	pure ()

readHeader :: (
	Union.Member Pipe.P effs,
	Union.Member (State.S Request) effs,
	Union.Member (State.S Crc) effs,
	Union.Member (Except.E String) effs,
	Union.Member Fail.F effs ) =>
	Eff.E effs BS.ByteString o GzipHeader
readHeader = do
	(_, foo) <- crcPipe Pipe.=$= do
		State.put $ RequestBytes 2
		ids <- Pipe.await
		when (ids /= "\31\139")
			$ Except.throw @String "Bad magic"
		State.put $ RequestBytes 1
		cm <- (CompressionMethod . BS.head) <$> Pipe.await
		Just flgs <- (readFlags . BS.head) <$> Pipe.await
		State.put $ RequestBytes 4
		mtm <- (word32ToCTime . bsToNum) <$> Pipe.await
		State.put $ RequestBytes 1
		ef <- BS.head <$> Pipe.await
		os <- OS . BS.head <$> Pipe.await
		mexflds <- if (flagsRawExtra flgs)
		then do
			State.put $ RequestBytes 2
			xlen <- bsToWord16 <$> Pipe.await
			State.put . RequestBytes $ fromIntegral xlen
			exflgs <- decodeExtraFields <$> Pipe.await
			pure exflgs
		else pure []
		State.put RequestString
		mnm <- if (flagsRawName flgs)
		then do	nm <- Pipe.await
			pure $ Just nm
			else pure Nothing
		mcmmt <- if (flagsRawComment flgs)
		then do
			cmmt <- Pipe.await
			pure $ Just cmmt
		else pure Nothing
		when (flagsRawHcrc flgs) do
			compCrc
			crc <- (\(Crc c) -> fromIntegral $ (.&. 0xffff) c) <$> State.get @Crc
			State.put $ RequestBytes 2
			crc0 <- bsToNum @Word16 <$> Pipe.await
			when (crc /= crc0) $
				Except.throw @String "Header CRC check failed"
		pure GzipHeader {
			gzipHeaderCompressionMethod = cm,
			gzipHeaderFlags = Flags {
				flagsText = flagsRawText flgs,
				flagsHcrc = flagsRawHcrc flgs },
			gzipHeaderModificationTime = mtm, gzipHeaderExtraFlags = ef,
			gzipHeaderOperatingSystem = os, gzipHeaderExtraField = mexflds,
			gzipHeaderFileName = mnm, gzipHeaderComment = mcmmt }
	case foo of
		HFreer.Pure x -> pure x

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

data Flags = Flags {
	flagsText :: Bool,
	flagsHcrc :: Bool }
	deriving Show

data ExtraField = ExtraField {
	extraFieldSi1 :: Word8,
	extraFieldSi2 :: Word8,
	extraFieldData :: BS.ByteString }
	deriving Show

readFlags :: Word8 -> Maybe FlagsRaw
readFlags w = if or $ (w `testBit`) <$> [5 .. 7]
	then Nothing
	else Just FlagsRaw {
		flagsRawText = w `testBit` 0,
		flagsRawHcrc = w `testBit` 1,
		flagsRawExtra = w `testBit` 2,
		flagsRawName = w `testBit` 3,
		flagsRawComment = w `testBit` 4 }

data FlagsRaw = FlagsRaw {
	flagsRawText :: Bool,
	flagsRawHcrc :: Bool,
	flagsRawExtra :: Bool,
	flagsRawName :: Bool,
	flagsRawComment :: Bool }
	deriving Show

word32ToCTime :: Word32 -> CTime
word32ToCTime = CTime . fromIntegral

bsToWord16 :: BS.ByteString -> Word16
bsToWord16 bs = w0 .|. w1 `shiftL` 8
	where [w0, w1] = fromIntegral <$> BS.unpack bs

decodeExtraFields :: BS.ByteString -> [ExtraField]
decodeExtraFields "" = []
decodeExtraFields bs = let
	(ef, bs') = decodeExtraField bs in
	ef : decodeExtraFields bs'

decodeExtraField :: BS.ByteString -> (ExtraField, BS.ByteString)
decodeExtraField bs = let
	([si1, si2], bs') = BS.unpack `first` BS.splitAt 2 bs
	(ln, bs'') = (fromIntegral . bsToWord16) `first` BS.splitAt 2 bs'
	(dt, bs''') = BS.splitAt ln bs'' in (
		ExtraField {
			extraFieldSi1 = si1,
			extraFieldSi2 = si2,
			extraFieldData = dt },
		bs''' )
