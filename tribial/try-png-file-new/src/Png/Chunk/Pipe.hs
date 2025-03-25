{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Png.Chunk.Pipe where

import Control.Monad
import Control.Monad.Fix
import Control.MonadClasses.State
import Control.MonadClasses.Except
import Data.Bits
import Data.Pipe
import Data.ByteString qualified as BS

import Crc

checkMagic :: (
	PipeClass p,
	MonadState BS.ByteString (p BS.ByteString ChunkBlock m),
	MonadError String (p BS.ByteString ChunkBlock m),
	Monad m) =>
	p BS.ByteString ChunkBlock m ()
checkMagic = do
	mg <- takeByteString 8
	when (mg /= magic) $ throwError ("bad magic" :: String)

magic :: BS.ByteString
magic = "\x89\x50\x4e\x47\x0d\x0a\x1a\x0a"

chunk :: (
	PipeClass p,
	MonadState BS.ByteString (p BS.ByteString ChunkBlock m),
	MonadError String (p BS.ByteString ChunkBlock m),
	Monad m) =>
	p BS.ByteString ChunkBlock m ()
chunk = dataLength >>= \case
	Just rst0 -> do
		nm <- takeByteString 4
		yield $ ChunkBegin nm
		cr' <- ($ step' 0xffffffff nm) . ($ rst0) $ fix \go rst cr -> do
			bs <- flushByteString
			let	ln = BS.length bs
			if ln >= rst
			then do	yield (ChunkBody $ BS.take rst bs)
				put (BS.drop rst bs)
				pure (step' cr $ BS.take rst bs)
			else yield (ChunkBody bs) >> go (rst - ln) (step' cr bs)
		c <- takeByteString 4
		when (complement (step' cr' $ BS.reverse c) /= 0x2144df1c) $ throwError ("bad CRC" :: String)
		yield ChunkEnd
		pure ()
	Nothing -> pure ()

dataLength :: (
	PipeClass p,
	MonadState BS.ByteString (p BS.ByteString a m), Monad m ) =>
	p BS.ByteString a m (Maybe Int)
dataLength = bsToNum32 <$> takeByteString 4

bsToNum32 :: (Bits n, Integral n) => BS.ByteString -> Maybe n
bsToNum32 bs
	| BS.length bs == 4 = Just . bigEndian 0 . (fromIntegral <$>) $ BS.unpack bs
	| otherwise = Nothing

bigEndian :: Bits n => n -> [n] -> n
bigEndian s [] = s
bigEndian s (n : ns) = bigEndian (s `shiftL` 8 .|. n) ns

num32ToBs :: (Bits n, Integral n) => n -> BS.ByteString
num32ToBs nm = BS.replicate (4 - BS.length bs) 0 `BS.append` bs
	where
	bs = BS.pack $ be [] nm
	be ws = \case
		0 -> ws
		n -> be ((fromIntegral $ n .&. 0xff) : ws) (n `shiftR` 8)

data ChunkBlock
	= ChunkBegin BS.ByteString | ChunkEnd | ChunkBody BS.ByteString
	deriving Show

takeByteString :: (
	PipeClass p,
	MonadState BS.ByteString (p BS.ByteString a m), Monad m ) =>
	Int -> p BS.ByteString a m BS.ByteString
takeByteString n = do
	obs <- get
	let	ln = BS.length obs
	if ln >= n
	then BS.take n obs <$ modify (BS.drop n)
	else await >>= \case
		Just nbs -> if (n - ln > BS.length nbs)
			then do	put ("" :: BS.ByteString)
				bs' <- takeByteString (n - ln - BS.length nbs)
				pure $ obs `BS.append` nbs `BS.append` bs'
			else obs `BS.append` BS.take (n - ln) nbs
				<$ put (BS.drop (n - ln) nbs)
		Nothing -> pure obs

flushByteString :: (
	PipeClass p,
	MonadState BS.ByteString (p BS.ByteString a m), Monad m ) =>
	p BS.ByteString a m BS.ByteString
flushByteString = do
	obs <- get
	if BS.null obs
	then await >>= \case Just nbs -> pure nbs; Nothing -> pure ""
	else obs <$ put ("" :: BS.ByteString)
