{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
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

import Pipe.Gzip
import Pipe.IO

main :: IO ()
main = do
	fp : _ <- getArgs
	h <- openFile fp ReadMode
	(putStrLn . take 100 . show =<<)
		. run $ fromHandle (type ()) h Pipe.=$= do
			(Pipe.print' . gzipHeaderFromRaw =<< readHeader)
			mainPipe 10

run :: Eff.E (Pipe () () '[
	Except.E String, State.S Crc, State.S BS.ByteString,
	State.S BitInfo, State.S ExtraBits, State.S (BinTree Int, BinTree Int),
	Fail.F, IO ]) () ->
	IO (Either String (
		((((Either String (Maybe ()), Crc), BS.ByteString), BitInfo),
			ExtraBits), (BinTree Int, BinTree Int)) )
run = Eff.runM . Fail.run
	. (`State.run` (fixedTable, fixedTable)) . (`State.run` ExtraBits 0)
	. runBitArray "" . (`State.run` Crc 0xffffffff)
	. Except.run @String . Pipe.run @() @()

type Pipe i o effs = (Pipe.P i o ': effs)

mainPipe :: forall effs . (
	Union.Member (State.S BitInfo) effs,
	Union.Member (State.S BS.ByteString) effs,
	Union.Member (State.S (BinTree Int, BinTree Int)) effs,
	Union.Member (State.S ExtraBits) effs,
	Union.Member (Except.E String) effs,
	Union.Member Union.Fail effs, Union.Member IO effs ) =>
	Int -> Eff.E (Pipe BS.ByteString () effs) ()
mainPipe bffsz = do
	Pipe.print' =<< takeBit8 @() 1
	mbt <- takeBit8 @() 2
	let	bt = maybe 3 id mbt
	if bt == 0
	then readNonCompressed @() bffsz
	else if bt == 1 then bits Pipe.=$=
		huffmanPipe @(Pipe Bit (Either Int Word16) effs) Pipe.=$=
		putDecoded fixedTable fixedDstTable
	else if bt == 2 then do
		Just hlit <- ((+ 257) . fromIntegral <$>) <$> takeBit8 @() 5
		Just hdist <- ((+ 1) . fromIntegral <$>) <$> takeBit8 @() 5
		Just hclen <- ((+ 4) . fromIntegral <$>) <$> takeBit8 @() 4
		bits Pipe.=$= do
			clcls <- mkTree @Word8 [16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15]
				<$> replicateM hclen (bitListToNumLE . catMaybes <$> replicateM 3 (Pipe.await @_ @()))
			State.put (clcls :: BinTree Int, clcls)
			huffmanPipe @(Pipe Bit (Either Int Word16) effs) Pipe.=$= do
				(lct, dct) <- (mkTree [0 ..] *** mkTree [0 ..]) .
					Prelude.splitAt hlit <$> getCodeTable (hlit + hdist)
				State.put (lct, lct :: BinTree Int)
				putDecoded lct dct
	else error "not implemented"

printNoLiteral :: (
	Show a, Show b, Ord a, Num a,
	Union.Member IO effs ) =>
	Either a b -> Eff.E effs ()
printNoLiteral (Right i) = putStrLn' $ "Right " ++ show i
printNoLiteral (Left i)
	| 0 <= i && i <= 255 = pure ()
	| otherwise = putStrLn' $ "Left " ++ show i

putDecoded :: (
	Union.Member (Pipe.P (Either Int Word16) ()) effs,
	Union.Member IO effs,
	Union.Member (State.S (BinTree Int, BinTree Int)) effs,
	Union.Member (State.S ExtraBits) effs
	) =>
	BinTree Int -> BinTree Int -> Eff.E effs ()
putDecoded t dt = do
	mi <- Pipe.await @(Either Int Word16) @()
	maybe (pure ()) printNoLiteral mi
	case mi of
		Just (Left 256) -> pure ()
		Just (Left i)
			| 0 <= i && i <= 255 -> putChar' (chr i) >> putDecoded t dt
			| 257 <= i && i <= 264 -> State.put (dt, dt) >> putDist t dt
			| 265 <= i && i <= 284 -> do
				State.put . ExtraBits $ (i - 261) `div` 4
				putDecoded t dt
			| i == 285 -> State.put (dt, dt) >> putDist t dt
			| otherwise -> error $ "putDecoded: yet " ++ show i
		Just (Right _) -> do
			State.put (dt, dt)
			putDist t dt
		Nothing -> pure ()
	where putChar' = Eff.eff . putChar

putDist :: (
	Union.Member (Pipe.P (Either Int Word16) ()) effs,
	Union.Member IO effs,
	Union.Member (State.S (BinTree Int, BinTree Int)) effs,
	Union.Member (State.S ExtraBits) effs
	) =>
	BinTree Int -> BinTree Int -> Eff.E effs ()
putDist t dt = do
	mi <- Pipe.await @(Either Int Word16) @()
	Pipe.print' mi
	case mi of
		Just (Left i)
			| 0 <= i && i <= 3 -> State.put (t, t) >> putDecoded t dt
			| 4 <= i && i <= 29 -> do
				State.put . ExtraBits $ (i - 2) `div` 2
				putDist t dt
			| otherwise -> error $ "putDist: yet " ++ show i
		Just (Right _) -> do
			State.put (t, t)
			putDecoded t dt
		_ -> error $ "putDist: yet"

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

readNonCompressed :: forall o effs . (
	Union.Member (Pipe.P BS.ByteString o) effs,
	Union.Member (State.S BitInfo) effs,
	Union.Member (State.S BS.ByteString) effs,
	Union.Member (Except.E String) effs,
	Union.Member Fail.F effs,
	Union.Member IO effs ) =>
	Int -> Eff.E effs ()
readNonCompressed bffsz = do
	Pipe.print' =<< takeByteBoundary @o
	ln <- takeWord16FromPair
	forM_ (separate bffsz ln) \ln' -> Pipe.print' =<< takeBytes @o ln'
	where
	takeWord16FromPair = do
		Just ln <- (bsToNum <$>) <$> takeBytes @o 2
		Just nln <- (bsToNum <$>) <$> takeBytes @o 2
		when ((ln `xor` complement nln) .&. 0xffff /= 0)
			$ Except.throw @String "bad boo"
		pure ln
	separate bs ln
		| ln == 0 = [] | ln <= bs = [ln]
		| otherwise = bs : separate bs (ln - bs)
