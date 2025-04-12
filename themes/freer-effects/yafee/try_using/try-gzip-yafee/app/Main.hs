{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, RequiredTypeArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main) where

import Control.Monad.Fix
import Control.Monad.Yafee.Eff qualified as Eff
import Control.Monad.Yafee.State qualified as State
import Control.Monad.Yafee.Except qualified as Except
import Control.Monad.Yafee.Pipe qualified as Pipe
import Control.Monad.Yafee.Fail qualified as Fail
import Control.OpenUnion qualified as Union
import Data.Foldable
import Data.Sequence qualified as Seq
import Data.Bool
import Data.Word
import Data.ByteString qualified as BS
import System.IO
import System.Environment

import Pipe.ByteString
import Pipe.ByteString.IO

import Gzip
import BitArray hiding (readMore')

import HuffmanTree
import Pipe.Huffman
import ByteStringNum

import Pipe.Gzip


import Numeric

import Block

formatSize :: Int
formatSize = 100

main :: IO ()
main = do
	fp : _ <- getArgs
	h <- openFile fp ReadMode
	(putStrLn . take 1000 . show =<<)
		. run $ fromHandle h Pipe.=$= do
			(Pipe.print' . gzipHeaderFromRaw =<< readHeader)
			mainPipe formatSize
			Pipe.print' =<< takeByteBoundary @()
			Pipe.print' . ((`showHex` "") . bsToNum @Word32 <$>) =<< takeBytes @() 4
			Pipe.print' . (bsToNum @Word32 <$>) =<< takeBytes @() 4

run :: Eff.E (Pipe () () '[
	Except.E String, State.Named "file-length" Int,
	State.S Crc,
	State.S BitArray, State.S ExtraBits, State.S (BinTree Int, BinTree Int),
	State.S (Seq.Seq Word8),
	State.Named "format" BS.ByteString,
	Fail.F, IO ]) () ->
	IO (Either String
		(((((((Either String ((), [()]), Int), Crc), BitArray),
						ExtraBits),
					(BinTree Int, BinTree Int)),
				Seq.Seq Word8),
			BS.ByteString))
run = Eff.runM . Fail.run
	. (`State.runN` "")
	. (`State.run` Seq.empty)
	. (`State.run` (fixedTable, fixedTable)) . (`State.run` ExtraBits 0)
	. runBitArray "" . (`State.run` Crc 0xffffffff) . (flip (State.runN @"file-length") 0)
	. Except.run @String . Pipe.run @() @()

mainPipe :: forall effs . (
	Union.Member (State.S BitArray) effs,
	Union.Member (State.S (BinTree Int, BinTree Int)) effs,
	Union.Member (State.S ExtraBits) effs,
	Union.Member (Except.E String) effs,
	Union.Member Union.Fail effs, Union.Member IO effs,
	Union.Member (State.S (Seq.Seq Word8)) effs,
	Union.Member (State.Named "format" BS.ByteString) effs,
	Union.Member (State.S Crc) effs,
	Union.Member (State.Named "file-length" Int) effs
	) =>
	Int -> Eff.E (Pipe BS.ByteString () effs) ()
mainPipe bffsz =
	fix (\go -> readBlock bffsz >>= bool (pure ()) go) Pipe.=$=
	runLength @(Pipe RunLength (Either Word8 BS.ByteString) effs) Pipe.=$=
	format @(Pipe (Either Word8 BS.ByteString) BS.ByteString effs) bffsz Pipe.=$=
	printPipe @() @(Pipe BS.ByteString () effs)

printPipe :: forall o effs . (
	Union.Member (Pipe.P BS.ByteString o) effs,
	Union.Member IO effs,
	Union.Member (State.S Crc) effs,
	Union.Member (State.Named "file-length" Int) effs
	) =>
	Eff.E effs ()
printPipe = do
	State.put $ Crc 0xffffffff
	printPipe' @o
	State.modify compCrc

printPipe' :: forall o effs . (
	Union.Member (Pipe.P BS.ByteString o) effs,
	Union.Member IO effs,
	Union.Member (State.S Crc) effs,
	Union.Member (State.Named "file-length" Int) effs
	) =>
	Eff.E effs ()
printPipe' = do
	mx <- Pipe.await' o
	maybe (pure ())
		(\x -> do
			calcCrc' x
			State.modifyN "file-length" (+ BS.length x)
			Pipe.print' (x :: BS.ByteString)
			printPipe' @o) mx

runLength :: (
	Union.Member (Pipe.P RunLength (Either Word8 BS.ByteString)) effs,
	Union.Member (State.S (Seq.Seq Word8)) effs ) =>
	Eff.E effs ()
runLength = Pipe.await' (Either Word8 BS.ByteString) >>= \case
	Nothing -> pure ()
	Just rl -> runLengthRun @RunLength rl >> runLength

runLengthRun :: forall i effs . (
	Union.Member (Pipe.P i (Either Word8 BS.ByteString)) effs,
	Union.Member (State.S (Seq.Seq Word8)) effs ) =>
	RunLength -> Eff.E effs ()
runLengthRun = \case
	RunLengthLiteral w -> do
		State.modify (`snoc` w)
		Pipe.yield' @(Either Word8 BS.ByteString) i $ Left w
	RunLengthLenDist (RunLengthLength ln) (RunLengthDist d) -> do
		ws' <- State.gets \ws -> repetition ws ln d
		State.modify (`appendR` ws')
		Pipe.yield' @(Either Word8 BS.ByteString) i . Right $ BS.pack ws'

snoc :: Seq.Seq Word8 -> Word8 -> Seq.Seq Word8
snoc s w = let s' = if ln > 32768 then Seq.drop (ln - 32768) s else s in
	s' Seq.|> w
	where ln = Seq.length s

appendR :: Seq.Seq Word8 -> [Word8] -> Seq.Seq Word8
appendR s ws = let s' = if ln > 32768 then Seq.drop (ln - 32768) s else s in
	foldl (Seq.|>) s' ws
	where ln = Seq.length s

repetition :: Seq.Seq Word8 -> Int -> Int -> [Word8]
repetition ws r d = takeRep r ws' ws'
	where ws' = toList . Seq.take r $ takeR d ws

takeRep :: Int -> [a] -> [a] -> [a]
takeRep 0 _ _ = []
takeRep n xs0 (x : xs) = x : takeRep (n - 1) xs0 xs
takeRep n xs0 [] = takeRep n xs0 xs0

takeR :: Int -> Seq.Seq Word8 -> Seq.Seq Word8
takeR n xs = Seq.drop (Seq.length xs - n) xs

format :: (
	Union.Member (Pipe.P (Either Word8 BS.ByteString) BS.ByteString) effs,
	Union.Member (State.Named "format" BS.ByteString) effs
	) =>
	Int -> Eff.E effs ()
format n = do
	b <- checkLength n
	if b
	then yieldLen @(Either Word8 BS.ByteString) n >> format n
	else readMore' >>= bool
		(Pipe.yield' (Either Word8 BS.ByteString) =<<
			State.getN @BS.ByteString "format")
		(format n)

readMore' :: (
	Union.Member (State.Named "format" BS.ByteString) effs,
	Union.Member (Pipe.P (Either Word8 BS.ByteString) BS.ByteString) effs
	) =>
	Eff.E effs Bool
readMore' = Pipe.await' BS.ByteString >>= \case
	Nothing -> pure False
	Just (Left w) -> True <$ State.modifyN "format" (`BS.snoc` w)
	Just (Right bs) -> True <$ State.modifyN "format" (`BS.append` bs)

checkLength :: (
	Union.Member (State.Named "format" BS.ByteString) effs
	) =>
	Int -> Eff.E effs Bool
checkLength n = do
	bs <- State.getN "format"
	pure $ BS.length bs >= n

yieldLen :: forall i effs . (
	Union.Member (State.Named "format" BS.ByteString) effs,
	Union.Member (Pipe.P i BS.ByteString) effs ) =>
	Int -> Eff.E effs ()
yieldLen n = do
	bs <- State.getN "format"
	let	(r, bs') = BS.splitAt n bs
	State.putN "format" bs'
	Pipe.yield' i r
