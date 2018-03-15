{-# LANGUAGE OverloadedStrings, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module ReadWav (Wav, wavData, analyze) where

import Control.Monad (join)
import Control.Monad.State (StateT(..), evalStateT, put)
import Data.Bits (Bits, shift)
import Data.Word (Word16, Word32)
import Data.Int (Int16)

import qualified Data.ByteString as BS

import WavType (
	Wav, readWav, wavData, AudioFormat, readAudioFormat, Err(..) )

type WavM = StateT BS.ByteString (Either Err)

bytes :: Int -> WavM BS.ByteString
bytes n = StateT $ \case
	"" -> Left Eof
	src -> Right $ BS.splitAt n src

littleNum :: (Bits a, Num a) => BS.ByteString -> a
littleNum = ln . BS.unpack
	where
	ln [] = 0
	ln (w : ws) = fromIntegral w + ln ws `shift` 8

getChunkId :: WavM BS.ByteString
getChunkId = bytes 4

getChunkSize :: WavM Word32
getChunkSize = littleNum <$> bytes 4

getFormat :: WavM BS.ByteString
getFormat = bytes 4

getSubchunk1Id :: WavM BS.ByteString
getSubchunk1Id = bytes 4

getSubchunk1Size :: WavM Word32
getSubchunk1Size = littleNum <$> bytes 4

getAudioFormat :: WavM AudioFormat
getAudioFormat = readAudioFormat . littleNum <$> bytes 2

getNumChannels :: WavM Word16
getNumChannels = littleNum <$> bytes 2

getSampleRate :: WavM Word32
getSampleRate = littleNum <$> bytes 4

getByteRate :: WavM Word32
getByteRate = littleNum <$> bytes 4

getBlockAlign :: WavM Word16
getBlockAlign = littleNum <$> bytes 2

getBitsPerSample :: WavM Word16
getBitsPerSample = littleNum <$> bytes 2

getSubchunk2Id :: WavM BS.ByteString
getSubchunk2Id = bytes 4

getSubchunk2Size :: WavM Word32
getSubchunk2Size = littleNum <$> bytes 4

analyze :: BS.ByteString -> Either Err Wav
analyze src = join . (`evalStateT` src) $ readWav
	<$> getChunkId
	<*> getChunkSize
	<*> getFormat
	<*> getSubchunk1Id
	<*> getSubchunk1Size
	<*> getAudioFormat
	<*> getNumChannels
	<*> getSampleRate
	<*> getByteRate
	<*> getBlockAlign
	<*> getBitsPerSample
	<*> getSubchunk2Id
	<*> ( do
		sz <- getSubchunk2Size
		bytes (fromIntegral sz) >>= put
		analyzeData
		)

catchEof :: WavM a -> WavM (Maybe a)
catchEof act = StateT $ \s -> case runStateT act s of
	Right (x, s') -> Right (Just x, s')
	Left Eof -> Right (Nothing, s)
	Left e -> Left e

doWhile :: Monad m => m (Maybe a) -> m [a]
doWhile act = do
	mr <- act
	case mr of
		Just r -> (r :) <$> doWhile act
		Nothing -> return []

analyzeData :: WavM [Int16]
analyzeData = doWhile . catchEof $ littleNum <$> bytes 2
