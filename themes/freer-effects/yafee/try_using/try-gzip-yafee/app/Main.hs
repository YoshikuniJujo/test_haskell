{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, RequiredTypeArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Arrow
import Control.Monad
import Control.Monad.Yafee.Eff qualified as Eff
import Control.Monad.Yafee.State qualified as State
import Control.Monad.Yafee.Except qualified as Except
import Control.Monad.Yafee.Pipe qualified as Pipe
import Control.Monad.Yafee.Fail qualified as Fail
import Control.OpenUnion qualified as Union
import Data.Bits
import Data.Maybe
import Data.List qualified as L
import Data.Word
import Data.Char
import Data.ByteString qualified as BS
import System.IO
import System.Environment

import Pipe.ByteString
import Pipe.ByteString.IO

import Gzip
import BitArray

import HuffmanTree
import Pipe.Huffman
import ByteStringNum

main :: IO ()
main = do
	fp : _ <- getArgs
	h <- openFile fp ReadMode
	(putStrLn . take 100 . show =<<) . Eff.runM . Fail.run
		. (`State.run` (fixedTable, fixedTable))
		. (`State.run` ExtraBits 0)
		. (runBitArray "") . (`State.run` Crc 0xffffffff) . Except.run @String . Pipe.run @() @() $
		fromHandle (type ()) h Pipe.=$= do
			Pipe.print' . gzipHeaderFromRaw =<< readHeader
			Pipe.print' =<< takeBit8 @() 1
			mbt <- takeBit8 @() 2
			let	bt = maybe 3 id mbt
			if bt == 1
			then bits @'[Pipe.P BS.ByteString Bit,
				Except.E String,
				State.S Crc,
				State.S BS.ByteString,
				State.S BitInfo,
				State.S ExtraBits,
				State.S (BinTree Int, BinTree Int),
				Fail.F,
				IO] Pipe.=$= huffmanPipe
					@'[Pipe.P Bit (Either Int Word16),
						Except.E String,
						State.S Crc,
						State.S BS.ByteString, State.S BitInfo,
						State.S ExtraBits,
						State.S (BinTree Int, BinTree Int),
						Fail.F,
						IO] Pipe.=$= putDecoded {- do
							printAll @(Either Int Word16)
								@'[Pipe.P (Either Int Word16) (),
									Except.E String,
									State.S Crc,
									State.S BS.ByteString, State.S BitInfo,
									State.S ExtraBits,
									State.S (BinTree Int, BinTree Int),
									Fail.F,
									IO]
									-}
			else if bt == 2 then do
				Pipe.print' @_ @(Maybe Int) =<< ((+ 257) . fromIntegral <$>) <$> takeBit8 @() 5
				Pipe.print' @_ @(Maybe Int) =<< ((+ 1) . fromIntegral <$>) <$> takeBit8 @() 5
				Pipe.print' @_ @(Maybe Int) =<< ((+ 4) . fromIntegral <$>) <$> takeBit8 @() 4
				bits @'[Pipe.P BS.ByteString Bit,
					Except.E String,
					State.S Crc,
					State.S BS.ByteString,
					State.S BitInfo,
					State.S ExtraBits,
					State.S (BinTree Int, BinTree Int),
					Fail.F,
					IO] Pipe.=$= do
					clcls <- fromList . pairToCodes @Word8 . L.sort . filter ((/= 0) . fst)
						. (`zip` [16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15])
						<$> replicateM 14 (bitListToNumLE . catMaybes <$> replicateM 3 (Pipe.await @_ @()))
					Pipe.print' (clcls :: BinTree Int)
					State.put (clcls, clcls)
					huffmanPipe
						@'[Pipe.P Bit (Either Int Word16),
							Except.E String,
							State.S Crc,
							State.S BS.ByteString, State.S BitInfo,
							State.S ExtraBits,
							State.S (BinTree Int, BinTree Int),
							Fail.F,
							IO] Pipe.=$= do
								lct <- fromList . pairToCodes
									. L.sort . filter ((/= 0) . fst)
									. (`zip` [0 ..]) <$> getCodeTable 282
								Pipe.print' lct

								dct <- fromList . pairToCodes
									. L.sort . filter ((/= 0) . fst)
									. (`zip` [0 ..]) <$> getCodeTable 23
								Pipe.print' dct

								State.put (lct, lct :: BinTree Int)
								-- Pipe.print' =<< Pipe.await @(Either Int Word16) @()
								printWhileLiteral
								State.put $ ExtraBits 1
								Pipe.print' =<< Pipe.await @(Either Int Word16) @()
								
								State.put (dct, dct :: BinTree Int)
								Pipe.print' =<< Pipe.await @(Either Int Word16) @()
								State.put $ ExtraBits 4
								Pipe.print' =<< Pipe.await @(Either Int Word16) @()

								State.put (lct, lct :: BinTree Int)
								printWhileLiteral
			else do
				Pipe.print' =<< takeByteBoundary @()
				Pipe.print' =<< takeBytes @() 2
				Pipe.print' =<< takeBytes @() 2
				putStrLn' "foobar"

putStrLn' :: Union.Member IO effs => String -> Eff.E effs ()
putStrLn' = Eff.eff . putStrLn

readHeader :: (
	Union.Member (State.S BS.ByteString) effs,
	Union.Member (State.S BitInfo) effs,
	Union.Member (State.S Crc) effs,
	Union.Member (Pipe.P BS.ByteString ()) effs,
	Union.Member (Except.E String) effs,
	Union.Member Fail.F effs,
	Union.Member IO effs ) =>
	Eff.E effs GzipHeaderRaw
readHeader = do
	mids <- takeBytes @() 2
	case mids of
		Just ids | ids == "\US\139" -> putStrLn' "good magic"
		_ -> Except.throw ("bad magic" :: String)
	Just cm <- popByte' @()
	fs <- maybe (Except.throw @String "bad flags") pure . (readFlags =<<) =<< popByte' @()
	Just mt <- takeWord32' @()
	Just efs <- popByte' @()
	Just os <- popByte' @()
	bs' <- if (flagsRawExtra fs)
	then do	Just xln <- (fromIntegral <$>) <$> takeWord16' @()
		Just bs <- takeBytes' @() xln
		pure bs
	else pure ""
	fn <- if (flagsRawName fs) then takeString' else pure Nothing
	mcmmt <- if (flagsRawComment fs) then takeString' else pure Nothing
	pure GzipHeaderRaw {
		gzipHeaderRawCompressionMethod = CompressionMethod cm,
		gzipHeaderRawFlags = fs,
		gzipHeaderRawModificationTime = word32ToCTime mt,
		gzipHeaderRawExtraFlags = efs,
		gzipHeaderRawOperatingSystem = OS os,
		gzipHeaderRawExtraField = decodeExtraFields bs',
		gzipHeaderRawFileName = fn,
		gzipHeaderRawComment = mcmmt }

takeWord32' :: forall o effs . (
	Union.Member (State.S Crc) effs,
	Union.Member (State.S BS.ByteString) effs,
	Union.Member (State.S BitInfo) effs,
	Union.Member (Pipe.P BS.ByteString o) effs ) => Eff.E effs (Maybe Word32)
takeWord32' = (bsToNum <$>) <$> takeBytes' @o 4

takeWord16' :: forall o effs . (
	Union.Member (State.S Crc) effs,
	Union.Member (State.S BS.ByteString) effs,
	Union.Member (State.S BitInfo) effs,
	Union.Member (Pipe.P BS.ByteString o) effs ) => Eff.E effs (Maybe Word16)
takeWord16' = (bsToNum <$>) <$> takeBytes' @o 2

spanUntil :: Word8 -> BS.ByteString -> Maybe (BS.ByteString, BS.ByteString)
spanUntil b0 bs = case BS.uncons bs of
	Nothing -> Nothing
	Just (b, bs')
		| b == b0 -> Just ("", bs')
		| otherwise -> ((b `BS.cons`) `first`) <$> spanUntil b0 bs'

takeString' :: (
	Union.Member (State.S Crc) effs,
	Union.Member (State.S BS.ByteString) effs,
	Union.Member (State.S BitInfo) effs,
	Union.Member (Pipe.P BS.ByteString ()) effs
	) =>
	Eff.E effs (Maybe BS.ByteString)
takeString' = State.gets (spanUntil 0) >>= \case
	Nothing -> do
		b <- readMore @()
		if b then takeString' else pure Nothing
	Just (t, d) -> Just t <$ (
		State.put d >> State.put (BitInfo 0 (BS.length d * 8)) >> calcCrc' (t `BS.snoc` 0)
		)

putDecoded :: (
	Union.Member (Pipe.P (Either Int Word16) ()) effs,
	Union.Member IO effs,
	Union.Member (State.S (BinTree Int, BinTree Int)) effs,
	Union.Member (State.S ExtraBits) effs
	) =>
	Eff.E effs ()
putDecoded = do
	mi <- Pipe.await @(Either Int Word16) @()
	maybe (pure ()) Pipe.print' mi
	case mi of
		Just (Left 256) -> pure ()
		Just (Left i)
			| 0 <= i && i <= 255 -> putDecoded
			| 257 <= i && i <= 264 -> State.put (fixedDstTable, fixedDstTable) >> putDist
			| 265 <= i && i <= 268 -> do
				State.put $ ExtraBits 1
				putDecoded
			| otherwise -> error "putDecoded: yet"
		Just (Right _) -> do
			State.put (fixedDstTable, fixedDstTable)
			putDist
		Nothing -> pure ()

putDist :: (
	Union.Member (Pipe.P (Either Int Word16) ()) effs,
	Union.Member IO effs,
	Union.Member (State.S (BinTree Int, BinTree Int)) effs,
	Union.Member (State.S ExtraBits) effs
	) =>
	Eff.E effs ()
putDist = do
	mi <- Pipe.await @(Either Int Word16) @()
	Pipe.print' mi
	case mi of
		Just (Left i)
			| 0 <= i && i <= 3 -> State.put (fixedTable, fixedTable) >> putDecoded
			| 4 <= i && i <= 5 -> do
				State.put $ ExtraBits 1
				putDist
			| otherwise -> error "putDist: yet"
		Just (Right _) -> do
			State.put (fixedTable, fixedTable)
			putDecoded
		_ -> error "putDist: yet"


bitListToNumLE :: (Num n, Bits n) => [Bit] -> n
bitListToNumLE = foldr (\b s -> (case b of O -> 0; I -> 1) .|. s `shiftL` 1) 0

getCodeTable :: (
	Union.Member (State.S ExtraBits) effs,
	Union.Member (Pipe.P (Either Int Word16) ()) effs,
	Union.Member Fail.F effs
	) =>
	Int -> Eff.E effs [Int]
getCodeTable 0 = pure []
getCodeTable n = Pipe.await @(Either Int Word16) @() >>= \case
	Nothing -> pure []
	Just (Left ln)
		| 0 <= ln && ln <= 15 -> (ln :) <$> getCodeTable (n - 1)
		| ln == 16 -> error "yet"
		| ln == 17 -> do
			State.put $ ExtraBits 3
			Just (Right eb) <- Pipe.await @(Either Int Word16) @()
			(replicate (fromIntegral eb + 3) 0 ++) <$> getCodeTable (n - fromIntegral eb - 3)
		| ln == 18 -> do
			State.put $ ExtraBits 7
			Just (Right eb) <- Pipe.await @(Either Int Word16) @()
			(replicate (fromIntegral eb + 11) 0 ++) <$> getCodeTable (n - fromIntegral eb - 11)
		| otherwise -> error "yet"
	Just (Right _) -> error "bad"

printWhileLiteral :: (
	Union.Member (Pipe.P (Either Int Word16) ()) effs,
	Union.Member IO effs
	) =>
	Eff.E effs ()
printWhileLiteral = Pipe.await @(Either Int Word16) @() >>= \case
	Just (Left i)
		| 0 <= i && i <= 255 -> do
			putChar' $ chr i
			printWhileLiteral
	mi -> putStrLn' "" >> Pipe.print' mi

putChar' :: Union.Member IO effs => Char -> Eff.E effs ()
putChar' = Eff.eff . putChar
