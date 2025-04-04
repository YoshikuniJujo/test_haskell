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
import Control.Monad.Yafe.Eff qualified as Eff
import Control.Monad.Yafe.State
import Control.Monad.Yafe.Except
import Control.Monad.Yafe.Pipe
import Control.Monad.Yafe.Fail
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
--	h <- openFile "../../../../../tribial/try-gzip/samples/abcd.txt.gz" ReadMode
	(putStrLn . take 100 . show =<<) . Eff.runM . runFail
		. (`runState` (fixedTable, fixedTable))
		. (`runState` ExtraBits 0)
		. (runBitArray "") . runError @String . runPipe @() @() $
		fromHandle (type ()) h =$= do
			print' =<< readHeader
			print' =<< takeBit8 @() 1
			mbt <- takeBit8 @() 2
			let	bt = maybe 3 id mbt
			if bt == 1
			then bits @'[Pipe BS.ByteString Bit,
				Exc String,
				State BS.ByteString,
				State BitInfo,
				State ExtraBits,
				State (BinTree Int, BinTree Int),
				Fail,
				IO] =$= huffmanPipe
					@'[Pipe Bit (Either Int Word16),
						Exc String,
						State BS.ByteString, State BitInfo,
						State ExtraBits,
						State (BinTree Int, BinTree Int),
						Fail,
						IO] =$= putDecoded {- do
							printAll @(Either Int Word16)
								@'[Pipe (Either Int Word16) (),
									Exc String,
									State BS.ByteString, State BitInfo,
									State ExtraBits,
									State (BinTree Int, BinTree Int),
									Fail,
									IO]
									-}
			else do	print' @_ @(Maybe Int) =<< ((+ 257) . fromIntegral <$>) <$> takeBit8 @() 5
				print' @_ @(Maybe Int) =<< ((+ 1) . fromIntegral <$>) <$> takeBit8 @() 5
				print' @_ @(Maybe Int) =<< ((+ 4) . fromIntegral <$>) <$> takeBit8 @() 4
				bits @'[Pipe BS.ByteString Bit,
					Exc String,
					State BS.ByteString,
					State BitInfo,
					State ExtraBits,
					State (BinTree Int, BinTree Int),
					Fail,
					IO] =$= do
					clcls <- fromList . pairToCodes @Word8 . L.sort . filter ((/= 0) . fst)
						. (`zip` [16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15])
						<$> replicateM 14 (bitListToNumLE . catMaybes <$> replicateM 3 (await @_ @()))
					print' (clcls :: BinTree Int)
					put (clcls, clcls)
					huffmanPipe
						@'[Pipe Bit (Either Int Word16),
							Exc String,
							State BS.ByteString, State BitInfo,
							State ExtraBits,
							State (BinTree Int, BinTree Int),
							Fail,
							IO] =$= do
								lct <- fromList . pairToCodes
									. L.sort . filter ((/= 0) . fst)
									. (`zip` [0 ..]) <$> getCodeTable 282
								print' lct

								dct <- fromList . pairToCodes
									. L.sort . filter ((/= 0) . fst)
									. (`zip` [0 ..]) <$> getCodeTable 23
								print' dct

								put (lct, lct :: BinTree Int)
								-- print' =<< await @(Either Int Word16) @()
								printWhileLiteral
								put $ ExtraBits 1
								print' =<< await @(Either Int Word16) @()
								
								put (dct, dct :: BinTree Int)
								print' =<< await @(Either Int Word16) @()
								put $ ExtraBits 4
								print' =<< await @(Either Int Word16) @()

								put (lct, lct :: BinTree Int)
								printWhileLiteral

putStrLn' :: Union.Member IO effs => String -> Eff.E effs ()
putStrLn' = Eff.eff . putStrLn

printAll :: forall i effs .
	(Union.Member (Pipe i ()) effs, Union.Member IO effs, Show i) =>
	Int -> Eff.E effs ()
printAll 0 = pure ()
printAll n = do
	mx <- await @i @()
	case mx of
		Nothing -> pure ()
		Just x -> Eff.eff (print x) >> printAll @i (n - 1)

readHeader :: (
	Union.Member (State BS.ByteString) effs,
	Union.Member (State BitInfo) effs,
	Union.Member (Pipe BS.ByteString ()) effs,
	Union.Member (Exc String) effs,
	Union.Member Fail effs,
	Union.Member IO effs ) =>
	Eff.E effs GzipHeader
readHeader = do
	mids <- takeBytes @() 2
	case mids of
		Just ids | ids == "\US\139" -> putStrLn' "good magic"
		_ -> throwError ("bad magic" :: String)
	Just cm <- popByte @()
	fs <- maybe (throwError @String "bad flags") pure . (readFlags =<<) =<< popByte @()
	Just mt <- takeWord32 @()
	Just efs <- popByte @()
	Just os <- popByte @()
	Just fn <- takeString
	pure GzipHeader {
		gzipHeaderCompressionMethod = cm,
		gzipHeaderFlags = fs,
		gzipHeaderModificationTime = mt,
		gzipExtraFlags = efs,
		gzipOperatingSystem = os,
		gzipFileName = fn }

takeWord32 :: forall o effs . (
	Union.Member (State BS.ByteString) effs,
	Union.Member (State BitInfo) effs,
	Union.Member (Pipe BS.ByteString o) effs ) => Eff.E effs (Maybe Word32)
takeWord32 = (bsToNum <$>) <$> takeBytes @o 4

spanUntil :: Word8 -> BS.ByteString -> Maybe (BS.ByteString, BS.ByteString)
spanUntil b0 bs = case BS.uncons bs of
	Nothing -> Nothing
	Just (b, bs')
		| b == b0 -> Just ("", bs')
		| otherwise -> ((b `BS.cons`) `first`) <$> spanUntil b0 bs'

takeString :: (
	Union.Member (State BS.ByteString) effs,
	Union.Member (State BitInfo) effs,
	Union.Member (Pipe BS.ByteString ()) effs
	) =>
	Eff.E effs (Maybe BS.ByteString)
takeString = gets (spanUntil 0) >>= \case
	Nothing -> do
		b <- readMore @()
		if b then takeString else pure Nothing
	Just (t, d) -> Just t <$ (put d >> put (BitInfo 0 (BS.length d * 8)))

putDecoded :: (
	Union.Member (Pipe (Either Int Word16) ()) effs,
	Union.Member IO effs,
	Union.Member (State (BinTree Int, BinTree Int)) effs,
	Union.Member (State ExtraBits) effs
	) =>
	Eff.E effs ()
putDecoded = do
	mi <- await @(Either Int Word16) @()
	maybe (pure ()) print' mi
	case mi of
		Just (Left 256) -> pure ()
		Just (Left i)
			| 0 <= i && i <= 255 -> putDecoded
			| 257 <= i && i <= 264 -> put (fixedDstTable, fixedDstTable) >> putDist
			| 265 <= i && i <= 268 -> do
				put $ ExtraBits 1
				putDecoded
			| otherwise -> error "putDecoded: yet"
		Just (Right _) -> do
			put (fixedDstTable, fixedDstTable)
			putDist
		Nothing -> pure ()

putDist :: (
	Union.Member (Pipe (Either Int Word16) ()) effs,
	Union.Member IO effs,
	Union.Member (State (BinTree Int, BinTree Int)) effs,
	Union.Member (State ExtraBits) effs
	) =>
	Eff.E effs ()
putDist = do
	mi <- await @(Either Int Word16) @()
	print' mi
	case mi of
		Just (Left i)
			| 0 <= i && i <= 3 -> put (fixedTable, fixedTable) >> putDecoded
			| 4 <= i && i <= 5 -> do
				put $ ExtraBits 1
				putDist
			| otherwise -> error "putDist: yet"
		Just (Right _) -> do
			put (fixedTable, fixedTable)
			putDecoded
		_ -> error "putDist: yet"


bitListToNumLE :: (Num n, Bits n) => [Bit] -> n
bitListToNumLE = foldr (\b s -> (case b of O -> 0; I -> 1) .|. s `shiftL` 1) 0

getCodeTable :: (
	Union.Member (State ExtraBits) effs,
	Union.Member (Pipe (Either Int Word16) ()) effs,
	Union.Member Fail effs
	) =>
	Int -> Eff.E effs [Int]
getCodeTable 0 = pure []
getCodeTable n = await @(Either Int Word16) @() >>= \case
	Nothing -> pure []
	Just (Left ln)
		| 0 <= ln && ln <= 15 -> (ln :) <$> getCodeTable (n - 1)
		| ln == 16 -> error "yet"
		| ln == 17 -> do
			put $ ExtraBits 3
			Just (Right eb) <- await @(Either Int Word16) @()
			(replicate (fromIntegral eb + 3) 0 ++) <$> getCodeTable (n - fromIntegral eb - 3)
		| ln == 18 -> do
			put $ ExtraBits 7
			Just (Right eb) <- await @(Either Int Word16) @()
			(replicate (fromIntegral eb + 11) 0 ++) <$> getCodeTable (n - fromIntegral eb - 11)
		| otherwise -> error "yet"
	Just (Right _) -> error "bad"

printWhileLiteral :: (
	Union.Member (Pipe (Either Int Word16) ()) effs,
	Union.Member IO effs
	) =>
	Eff.E effs ()
printWhileLiteral = await @(Either Int Word16) @() >>= \case
	Just (Left i)
		| 0 <= i && i <= 255 -> do
			putChar' $ chr i
			printWhileLiteral
	mi -> putStrLn' "" >> print' mi

putStr' :: Union.Member IO effs => String -> Eff.E effs ()
putStr' = Eff.eff . putStr

putChar' :: Union.Member IO effs => Char -> Eff.E effs ()
putChar' = Eff.eff . putChar
