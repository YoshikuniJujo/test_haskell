{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Foreign.C.Types
import Control.Arrow
import Control.Monad
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.List qualified as PipeL
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeB
import Control.Monad.Yaftee.Pipe.ByteString.OnDemand qualified as OnDemand
import Control.Monad.Yaftee.State qualified as State
import Control.Monad.Yaftee.Except qualified as Except
import Control.Monad.Yaftee.Fail qualified as Fail
import Data.Bits
import Data.Word
import Data.ByteString qualified as BS
import Data.ByteString.BitArray qualified as BitArray
import System.IO

main :: IO ()
main = do
	h <- openFile "sample/foo.txt.gz" ReadMode
	(print =<<)
		. Eff.runM
		. Fail.run
		. Except.run @String
		. (`State.run` OnDemand.RequestBuffer 16)
		. (`State.run` BitArray.fromByteString "")
		. PipeL.to
		$ PipeB.hGet' 64 h Pipe.=$= OnDemand.onDemand Pipe.=$= do
			PipeT.checkRight Pipe.=$= do
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
				mhcrc <- if flagsRawHcrc flgs
				then do	State.put $ OnDemand.RequestBytes 2
					Just <$> Pipe.await
				else pure Nothing
				Pipe.yield mhcrc

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
