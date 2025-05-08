{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Foreign.C.Types
import Control.Arrow
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.List qualified as PipeL
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeB
import Control.Monad.Yaftee.Pipe.ByteString.OnDemand qualified as OnDemand
import Control.Monad.Yaftee.Pipe.ByteString.Crc qualified as Crc
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.Fail qualified as Fail
import Control.Monad.Yaftee.IO qualified as IO
import Control.HigherOpenUnion qualified as U
import Data.Bits
import Data.Bool
import Data.Word
import Data.ByteString qualified as BS
import Data.ByteString.BitArray qualified as BitArray
import System.IO

main :: IO ()
main = do
	h <- openFile "sample/foo.txt.gz" ReadMode
	let	processHeader = IO.print
	(print =<<)
		. Eff.runM
		. Except.run @String
		. Fail.runExc id
		. (`State.run` OnDemand.RequestBuffer 16)
		. (`State.run` BitArray.fromByteString "")
		. (`State.run` Crc.Crc32 0)
		. PipeL.to
		$ PipeB.hGet' 64 h Pipe.=$= OnDemand.onDemand Pipe.=$= do
			_ <- PipeT.checkRight Pipe.=$= readHeader processHeader
			blocks

blocks :: (
	U.Member Pipe.P es,
	U.Member (State.S OnDemand.Request) es,
	U.Member (Except.E String) es, U.Member Fail.F es ) =>
	Eff.E es (Either BitArray.B BS.ByteString) BS.ByteString ()
blocks = fix \go -> block >>= bool (pure ()) go

block :: (
	U.Member Pipe.P es, U.Member (State.S OnDemand.Request) es,
	U.Member (Except.E String) es,
	U.Member (U.FromFirst U.Fail) es ) =>
	Eff.E es (Either BitArray.B BS.ByteString) BS.ByteString Bool
block = do
	State.put $ OnDemand.RequestBits 1
	Just bf <- either (Just . BitArray.toBits @Word8) (const Nothing) <$> Pipe.await
	State.put $ OnDemand.RequestBits 2
	Just bt <- either (Just . BitArray.toBits @Word8) (const Nothing) <$> Pipe.await
	case bt of
		0 -> do
			State.put $ OnDemand.RequestBytes 4
			ln <- getWord16FromPair =<< skipLeft1
			State.put $ OnDemand.RequestBytes ln
			Pipe.yield =<< getRight =<< Pipe.await
		_ -> Except.throw "yet"
	pure (bf /= 1)

getRight :: U.Member (Except.E String) es => Either a b -> Eff.E es i o b
getRight (Left _) = Except.throw "getRight: Left _"
getRight (Right r) = pure r

readHeader :: (
	U.Member Pipe.P es,
	U.Member (State.S Crc.Crc32) es,
	U.Member (State.S OnDemand.Request) es,
	U.Member (Except.E String) es,
	U.Member (U.FromFirst U.Fail) es ) =>
	(GzipHeader -> Eff.E es BS.ByteString o r) ->
	Eff.E es BS.ByteString o (
		Eff.E es BS.ByteString BS.ByteString r1,
		Eff.E es BS.ByteString o r )
readHeader f = Crc.crc32 Pipe.=$= do
				State.put $ OnDemand.RequestBytes 2
				ids <- Pipe.await
				when (ids /= "\31\139")
					$ Except.throw @String "Bad magic"
				State.put $ OnDemand.RequestBytes 1
				cm <- (CompressionMethod . BS.head) <$> Pipe.await
				Just flgs <- readFlags . BS.head <$> Pipe.await
				State.put $ OnDemand.RequestBytes 4
				mtm <- word32ToCTime . bsToNum <$> Pipe.await
				State.put $ OnDemand.RequestBytes 1
				ef <- BS.head <$> Pipe.await
				os <- OS . BS.head <$> Pipe.await
				mexflds <- if (flagsRawExtra flgs)
				then do	State.put $ OnDemand.RequestBytes 2
					xlen <- bsToNum <$> Pipe.await
					State.put $ OnDemand.RequestBytes xlen
					decodeExtraFields <$> Pipe.await
				else pure []
				State.put OnDemand.RequestString
				mnm <- if flagsRawName flgs
				then Just <$> Pipe.await
				else pure Nothing
				mcmmt <- if flagsRawComment flgs
				then Just <$> Pipe.await
				else pure Nothing
				when (flagsRawHcrc flgs) do
					Crc.compCrc32
					crc <- (.&. 0xffff) . (\(Crc.Crc32 c) -> c) <$> State.get
					State.put $ OnDemand.RequestBytes 2
					m <- bsToNum <$> Pipe.await
					when (crc /= m) $
						Except.throw @String "Header CRC check failed"
				f GzipHeader {
					gzipHeaderCompressionMethod = cm,
					gzipHeaderFlags = Flags {
						flagsText = flagsRawText flgs,
						flagsHcrc = flagsRawHcrc flgs },
					gzipHeaderModificationTime = mtm,
					gzipHeaderExtraFlags = ef,
					gzipHeaderOperatingSystem = os,
					gzipHeaderExtraField = mexflds,
					gzipHeaderFileName = mnm,
					gzipHeaderComment = mcmmt }

newtype CompressionMethod = CompressionMethod {
	unCompressionMeghod :: Word8 }

pattern CompressionMethodDeflate :: CompressionMethod
pattern CompressionMethodDeflate = CompressionMethod 8

instance Show CompressionMethod where
	show CompressionMethodDeflate = "CompressionMethodDeflate"
	show (CompressionMethod cm) = "(CompressionMethod " ++ show cm ++ ")"

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

data Flags = Flags {
	flagsText :: Bool,
	flagsHcrc :: Bool }
	deriving Show

bsToNum :: (Bits n, Integral n) => BS.ByteString -> n
bsToNum = foldr (\b s -> fromIntegral b .|. s `shiftL` 8) 0 . BS.unpack

word32ToCTime :: Word32 -> CTime
word32ToCTime = CTime . fromIntegral

newtype OS = OS { unOS :: Word8 }

pattern OSUnix :: OS
pattern OSUnix = OS 3

instance Show OS where
	show OSUnix = "OSUnix"
	show (OS os) = "(OS " ++ show os ++ ")"

decodeExtraFields :: BS.ByteString -> [ExtraField]
decodeExtraFields "" = []
decodeExtraFields bs = let
	(ef, bs') = decodeExtraField bs in
	ef : decodeExtraFields bs'

decodeExtraField :: BS.ByteString -> (ExtraField, BS.ByteString)
decodeExtraField bs = let
	([si1, si2], bs') = BS.unpack `first` BS.splitAt 2 bs
	(ln, bs'') = bsToNum `first` BS.splitAt 2 bs'
	(dt, bs''') = BS.splitAt ln bs'' in (
		ExtraField {
			extraFieldSi1 = si1,
			extraFieldSi2 = si2,
			extraFieldData = dt },
		bs''' )

data ExtraField = ExtraField {
	extraFieldSi1 :: Word8,
	extraFieldSi2 :: Word8,
	extraFieldData :: BS.ByteString }
	deriving Show

data GzipHeader = GzipHeader {
	gzipHeaderCompressionMethod :: CompressionMethod,
	gzipHeaderFlags :: Flags,
	gzipHeaderModificationTime :: CTime,
	gzipHeaderExtraFlags :: Word8,
	gzipHeaderOperatingSystem :: OS,
	gzipHeaderExtraField :: [ExtraField],
	gzipHeaderFileName :: Maybe BS.ByteString,
	gzipHeaderComment :: Maybe BS.ByteString }
	deriving Show

skipLeft1 :: (U.Member Pipe.P es, U.Member (Except.E String) es) =>
	Eff.E es (Either a b) o b
skipLeft1 = Pipe.await >>= \case
	Left _ -> Pipe.await >>= \case
		Left _ -> Except.throw @String "Not Right"
		Right x -> pure x
	Right x -> pure x

getWord16FromPair :: (U.Member (Except.E String) es, Num n) =>
	BS.ByteString -> Eff.E es i o n
getWord16FromPair bs0 = fromIntegral @Word16 <$> do
	when (BS.length bs0 /= 4)
		$ Except.throw @String "getWord16FromPair: not 4 bytes"
	when (ln /= complement cln)
		$ Except.throw @String "bad pair"
	pure ln
	where
	(ln, cln) = (tow16 *** tow16) $ BS.splitAt 2 bs0
	tow16 bs = case BS.unpack bs of
		[b0, b1] -> fromIntegral b0 .|. (fromIntegral b1) `shiftL` 8
		_ -> error "never occur"
