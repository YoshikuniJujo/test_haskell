{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, RequiredTypeArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main (main, takeRep) where

import Control.Arrow
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Yafee.Eff qualified as Eff
import Control.Monad.Yafee.State qualified as State
import Control.Monad.Yafee.Except qualified as Except
import Control.Monad.Yafee.Pipe qualified as Pipe
import Control.Monad.Yafee.Fail qualified as Fail
import Control.OpenUnion qualified as Union
import Data.Foldable
import Data.Bits
import Data.Maybe
import Data.Sequence qualified as Seq
import Data.Bool
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

import Pipe.Gzip

import Calc

import Numeric

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
	State.S Crc, State.S BS.ByteString,
	State.S BitInfo, State.S ExtraBits, State.S (BinTree Int, BinTree Int),
	State.S (Seq.Seq Word8),
	State.Named "format" BS.ByteString,
	Fail.F, IO ]) () ->
	IO (Either String
		((((((((Either String ((), [()]), Int), Crc), BS.ByteString), BitInfo),
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

type Pipe i o effs = (Pipe.P i o ': effs)

toRunLength :: (
	Union.Member (Pipe.P BS.ByteString RunLength) effs
	) =>
	Eff.E effs ()
toRunLength = Pipe.await' RunLength >>= \case
	Nothing -> pure ()
	Just bs -> for_ (BS.unpack bs) $ Pipe.yield' BS.ByteString . RunLengthLiteral

mainPipe :: forall effs . (
	Union.Member (State.S BitInfo) effs,
	Union.Member (State.S BS.ByteString) effs,
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

readBlock :: forall effs . (
	Union.Member (State.S BitInfo) effs,
	Union.Member (State.S BS.ByteString) effs,
	Union.Member (State.S (BinTree Int, BinTree Int)) effs,
	Union.Member (State.S ExtraBits) effs,
	Union.Member (Except.E String) effs,
	Union.Member Union.Fail effs, Union.Member IO effs,
	Union.Member (State.S Crc) effs,
	Union.Member (State.Named "file-length" Int) effs
	) =>
	Int -> Eff.E (Pipe BS.ByteString RunLength effs) Bool
readBlock bffsz = do
	Just f <- takeBit8 @RunLength 1
	mbt <- takeBit8 @RunLength 2
	let	bt = maybe 3 id mbt
	if bt == 0
	then readNonCompressed bffsz Pipe.=$=
		toRunLength @(Pipe BS.ByteString RunLength effs)
	else do
		mfoo <- if bt == 2
			then do
				Just hlit <- ((+ 257) . fromIntegral <$>) <$> takeBit8 @RunLength 5
				Just hdist <- ((+ 1) . fromIntegral <$>) <$> takeBit8 @RunLength 5
				Just hclen <- ((+ 4) . fromIntegral <$>) <$> takeBit8 @RunLength 4
				pure $ Just (hlit, hdist, hclen)
			else pure Nothing

		bits Pipe.=$= do
			when (bt == 2) do
				let	Just (_, _, hclen) = mfoo
				clcls <- mkTree @Word8 [16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15]
					<$> replicateM hclen (bitListToNumLE . catMaybes <$> replicateM 3 (Pipe.await' RunLength))
				State.put (clcls :: BinTree Int, clcls)

			huffmanPipe @(Pipe Bit (Either Int Word16) effs) Pipe.=$= do

				(lct, dct) <- if (bt == 2)
					then do	let	Just (hlit, hdist, _) = mfoo
						(lct, dct) <- (mkTree [0 ..] *** mkTree [0 ..]) .
							Prelude.splitAt hlit <$> getCodeTable @RunLength (hlit + hdist)
						State.put (lct, lct :: BinTree Int)
						pure (lct, dct)
					else pure (fixedTable, fixedDstTable)

				putDecoded lct dct 0
	pure $ f /= 1

getCodeTable :: forall o effs . (
	Union.Member (State.S ExtraBits) effs,
	Union.Member (Pipe.P (Either Int Word16) o) effs,
	Union.Member Fail.F effs
	) =>
	Int -> Eff.E effs [Int]
getCodeTable 0 = pure []
getCodeTable n = Pipe.await' @(Either Int Word16) o >>= \case
	Nothing -> pure []
	Just (Left ln)
		| 0 <= ln && ln <= 15 -> (ln :) <$> getCodeTable @o (n - 1)
		| ln == 16 -> error "yet"
		| ln == 17 -> do
			State.put $ ExtraBits 3
			Just (Right eb) <- Pipe.await' @(Either Int Word16) o
			(replicate (fromIntegral eb + 3) 0 ++) <$> getCodeTable @o (n - fromIntegral eb - 3)
		| ln == 18 -> do
			State.put $ ExtraBits 7
			Just (Right eb) <- Pipe.await' @(Either Int Word16) o
			(replicate (fromIntegral eb + 11) 0 ++) <$> getCodeTable @o (n - fromIntegral eb - 11)
		| otherwise -> error "yet"
	Just (Right _) -> error "bad"

putDecoded :: (
	Union.Member (Pipe.P (Either Int Word16) RunLength) effs,
	Union.Member (State.S (BinTree Int, BinTree Int)) effs,
	Union.Member (State.S ExtraBits) effs
	) =>
	BinTree Int -> BinTree Int -> Int -> Eff.E effs ()
putDecoded t dt pri = do
	mi <- Pipe.await' @(Either Int Word16) RunLength
	case mi of
		Just (Left 256) -> pure ()
		Just (Left i)
			| 0 <= i && i <= 255 -> do
				Pipe.yield' (Either Int Word16) (RunLengthLiteral $ fromIntegral i)
				putDecoded t dt 0
			| 257 <= i && i <= 264 -> State.put (dt, dt) >> putDist t dt (runLengthLength i 0) 0
			| 265 <= i && i <= 284 -> do
				State.put . ExtraBits $ (i - 261) `div` 4
				putDecoded t dt i
			| i == 285 -> State.put (dt, dt) >> putDist t dt (runLengthLength i 0) 0
			| otherwise -> error $ "putDecoded: yet " ++ show i
		Just (Right eb) -> do
			State.put (dt, dt)
			putDist t dt (runLengthLength pri eb) 0
		Nothing -> pure ()

putDist :: (
	Union.Member (Pipe.P (Either Int Word16) RunLength) effs,
	Union.Member (State.S (BinTree Int, BinTree Int)) effs,
	Union.Member (State.S ExtraBits) effs
	) =>
	BinTree Int -> BinTree Int -> RunLengthLength -> Int -> Eff.E effs ()
putDist t dt ln pri = do
	mi <- Pipe.await' @(Either Int Word16) RunLength
--	Pipe.print' mi
	case mi of
		Just (Left i)
			| 0 <= i && i <= 3 -> do
				Pipe.yield' (Either Int Word16) (RunLengthLenDist ln (runLengthDist i 0))
				State.put (t, t)
				putDecoded t dt 0
			| 4 <= i && i <= 29 -> do
				State.put . ExtraBits $ (i - 2) `div` 2
				putDist t dt ln i
			| otherwise -> error $ "putDist: yet " ++ show i
		Just (Right eb) -> do
			Pipe.yield' (Either Int Word16) (RunLengthLenDist ln (runLengthDist pri eb))
			State.put (t, t)
			putDecoded t dt 0
		_ -> error $ "putDist: yet"

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

readNonCompressed :: forall effs . (
	Union.Member (State.S Crc) effs,
	Union.Member (State.Named "file-length" Int) effs,
	Union.Member (Pipe.P BS.ByteString BS.ByteString) effs,
	Union.Member (State.S BitInfo) effs,
	Union.Member (State.S BS.ByteString) effs,
	Union.Member (Except.E String) effs,
	Union.Member Fail.F effs,
	Union.Member IO effs ) =>
	Int -> Eff.E effs ()
readNonCompressed bffsz = do
	State.put $ Crc 0xffffffff
	Pipe.print' =<< takeByteBoundary @BS.ByteString
	ln <- takeWord16FromPair
	forM_ (separate bffsz ln) \ln' -> do
		bs <- takeBytes @BS.ByteString ln'
		State.modifyN "file-length" (+ ln')
		maybe (pure ()) (Pipe.yield' BS.ByteString) bs
	State.modify compCrc
	where
	takeWord16FromPair = do
		Just ln <- (bsToNum <$>) <$> takeBytes @BS.ByteString 2
		Just nln <- (bsToNum <$>) <$> takeBytes @BS.ByteString 2
		when ((ln `xor` complement nln) .&. 0xffff /= 0)
			$ Except.throw @String "bad boo"
		pure ln
	separate bs ln
		| ln == 0 = [] | ln <= bs = [ln]
		| otherwise = bs : separate bs (ln - bs)

data RunLength = RunLengthLiteral Word8 | RunLengthLenDist RunLengthLength RunLengthDist deriving Show

data RunLengthLength = RunLengthLength Int deriving Show

data RunLengthDist = RunLengthDist Int deriving Show

runLengthLength :: Int -> Word16 -> RunLengthLength
runLengthLength i eb = RunLengthLength $ calcLength i eb

runLengthDist :: Int -> Word16 -> RunLengthDist
runLengthDist i eb = RunLengthDist $ calcDist i eb

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
--		(pure ())
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
