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
import Control.Monad.Yafe.Eff qualified as Eff
import Control.Monad.Yafe.State
import Control.Monad.Yafe.Except
import Control.Monad.Yafe.Pipe
import Control.OpenUnion qualified as Union
import Data.Maybe
import Data.Word
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
	(putStrLn . take 100 . show =<<) . Eff.runM
		. (`runState` (fixedTable, fixedTable))
		. (`runState` ExtraBits 0)
		. (runBitArray "") . runError @String . runPipe @() @() $
		fromHandle (type ()) h =$= do
			print' =<< readHeader
			print' =<< takeBit8 @() 1
			print' =<< takeBit8 @() 2
			bits @'[Pipe BS.ByteString Bit,
				Exc String,
				State BS.ByteString,
				State BitInfo,
				State ExtraBits,
				State (BinTree Int, BinTree Int),
				IO] =$= huffmanPipe
					@'[Pipe Bit (Either Int Word16),
						Exc String,
						State BS.ByteString, State BitInfo,
						State ExtraBits,
						State (BinTree Int, BinTree Int),
						IO] =$= putDecoded {- do
							printAll @(Either Int Word16)
								@'[Pipe (Either Int Word16) (),
									Exc String,
									State BS.ByteString, State BitInfo,
									State ExtraBits,
									State (BinTree Int, BinTree Int),
									IO]
									-}

putStrLn' :: Union.Member IO effs => String -> Eff.E effs ()
putStrLn' = Eff.eff . putStrLn

printAll :: forall i effs .
	(Union.Member (Pipe i ()) effs, Union.Member IO effs, Show i) =>
	Eff.E effs ()
printAll = do
	mx <- await @i @()
	case mx of
		Nothing -> pure ()
		Just x -> Eff.eff (print x) >> printAll @i

readHeader :: (
	Union.Member (State BS.ByteString) effs,
	Union.Member (State BitInfo) effs,
	Union.Member (Pipe BS.ByteString ()) effs,
	Union.Member (Exc String) effs, Union.Member IO effs ) =>
	Eff.E effs GzipHeader
readHeader = do
	mids <- takeBytes @() 2
	case mids of
		Just ids | ids == "\US\139" -> putStrLn' "good magic"
		_ -> throwError ("bad magic" :: String)
	mcm <- popByte @()
	fs <- maybe (throwError @String "bad flags") pure . readFlags . fromJust =<< popByte @()
	mmt <- takeWord32 @()
	mefs <- popByte @()
	mos <- popByte @()
	mfn <- takeString
	pure GzipHeader {
		gzipHeaderCompressionMethod = fromJust mcm,
		gzipHeaderFlags = fs,
		gzipHeaderModificationTime = fromJust mmt,
		gzipExtraFlags = fromJust mefs,
		gzipOperatingSystem = fromJust mos,
		gzipFileName = fromJust mfn }

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
