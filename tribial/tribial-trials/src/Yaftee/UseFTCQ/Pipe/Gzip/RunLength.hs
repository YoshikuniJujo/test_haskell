{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Yaftee.UseFTCQ.Pipe.Gzip.RunLength (

	runLength, RunLength(..), RunLengthLength, RunLengthDist,

	runLengthToWord32,

	word32ToRunLength,

	toLitLenFreqs,
	toDistFreqs

	) where

import Control.Arrow
import Control.Monad.Fix
import Yaftee.UseFTCQ.Eff qualified as Eff
import Yaftee.UseFTCQ.Pipe qualified as Pipe
import Yaftee.UseFTCQ.State qualified as State
import Yaftee.OpenUnion qualified as Union
import Data.Foldable
import Data.List qualified as L
import Data.Word
import Data.Sequence qualified as Seq
import Data.ByteString qualified as BS

import Data.Calc

import Data.Bits

runLength :: (
	Union.Member Pipe.P effs,
	Union.Member (State.S (Seq.Seq Word8)) effs ) =>
	Eff.E effs RunLength (Either Word8 BS.ByteString) ()
runLength = Pipe.await >>= \rl -> (>> runLength) $ ($ rl) \case
	Literal w -> State.modify (`snoc` w) >> Pipe.yield (Left w)
	LiteralBS bs ->
		State.modify (`appendR` BS.unpack bs) >> Pipe.yield (Right bs)
	LenDist ln d -> State.gets (repetition ln d) >>= \ws ->
		State.modify (`appendR` ws) >> Pipe.yield (Right $ BS.pack ws)

word32ToRunLength :: Union.Member Pipe.P effs => Eff.E effs Word32 RunLength ()
word32ToRunLength = fix \go -> Pipe.await >>= \case
	w	| 0 <= w0 && w0 <= 255 -> do
			Pipe.yield . Literal $ fromIntegral w0
			go
		| otherwise -> Pipe.await >>= \case
			w' -> do
				Pipe.yield $ LenDist
					(calcLength (fromIntegral w0) (fromIntegral w1))
					(calcDist (fromIntegral w0') (fromIntegral w1'))
				go
				where
				w0' = w' .&. 0xffff
				w1' = w' `shiftR` 16
		where
		w0 = w .&. 0xffff
		w1 = w `shiftR` 16

data RunLength
	= Literal Word8 | LiteralBS BS.ByteString
	| LenDist RunLengthLength RunLengthDist | EndOfInput
	deriving Show

type RunLengthLength = Int

type RunLengthDist = Int

appendR :: Seq.Seq Word8 -> [Word8] -> Seq.Seq Word8
appendR s ws = let s' = if ln > 32768 then Seq.drop (ln - 32768) s else s in
	foldl (Seq.|>) s' ws
	where ln = Seq.length s

snoc :: Seq.Seq Word8 -> Word8 -> Seq.Seq Word8
snoc s w = let s' = if ln > 32768 then Seq.drop (ln - 32768) s else s in
	s' Seq.|> w
	where ln = Seq.length s

repetition :: Int -> Int -> Seq.Seq Word8 -> [Word8]
repetition r d ws = takeRep r ws' ws'
	where ws' = toList . Seq.take r $ takeR d ws

takeRep :: Int -> [a] -> [a] -> [a]
takeRep 0 _ _ = []
takeRep n xs0 (x : xs) = x : takeRep (n - 1) xs0 xs
takeRep n xs0 [] = takeRep n xs0 xs0

takeR :: Int -> Seq.Seq Word8 -> Seq.Seq Word8
takeR n xs = Seq.drop (Seq.length xs - n) xs

runLengthToWord32 :: RunLength -> [Word32]
runLengthToWord32 (LiteralBS bs) = fromLiteral' . fromIntegral <$> BS.unpack bs
runLengthToWord32 (Literal b) = [fromLiteral' $ fromIntegral b]
runLengthToWord32 (LenDist ln dst) = [fromLength' ln, fromDist' dst]

runLengthToLitLen :: RunLength -> [Int]
runLengthToLitLen (Literal b) = [fromIntegral b]
runLengthToLitLen (LiteralBS bs) = fromIntegral <$> BS.unpack bs
runLengthToLitLen (LenDist ln _dst) = [fst $ lengthToCode ln]
runLengthToLitLen EndOfInput = [256]

toLitLenFreqs :: [RunLength] -> [(Int, Int)]
toLitLenFreqs = ((head &&& length) <$>) . L.group . L.sort . (runLengthToLitLen =<<)

runLengthToDist :: RunLength -> [Int]
runLengthToDist (Literal _) = []
runLengthToDist (LiteralBS _) = []
runLengthToDist (LenDist _ln dst) = [fst $ distToCode dst]
runLengthToDist EndOfInput = []

toDistFreqs :: [RunLength] -> [(Int, Int)]
toDistFreqs = ((head &&& length) <$>) . L.group . L.sort . (runLengthToDist =<<)
