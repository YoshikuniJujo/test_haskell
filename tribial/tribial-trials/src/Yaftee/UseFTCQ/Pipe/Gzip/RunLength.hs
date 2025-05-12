{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fno-warn-x-partial #-}

module Yaftee.UseFTCQ.Pipe.Gzip.RunLength (

	runLength, runLength', R(..), Length, Dist,

	toLitLenFreqs,
	toDistFreqs

	) where

import Control.Arrow
import Yaftee.UseFTCQ.Eff qualified as Eff
import Yaftee.UseFTCQ.Pipe qualified as Pipe
import Yaftee.UseFTCQ.State qualified as State
import Yaftee.OpenUnion qualified as Union
import Data.Foldable
import Data.List qualified as L
import Data.Bool
import Data.Word
import Data.Sequence qualified as Seq
import Data.ByteString qualified as BS
import Data.Calc

runLength :: (
	Union.Member Pipe.P effs,
	Union.Member (State.S (Seq.Seq Word8)) effs ) =>
	Eff.E effs R (Either Word8 BS.ByteString) r
runLength = Pipe.await >>= \rl -> (>> runLength) $ ($ rl) \case
	Literal w -> State.modify (`snoc` w) >> Pipe.yield (Left w)
	LiteralBS bs ->
		State.modify (`appendR` BS.unpack bs) >> Pipe.yield (Right bs)
	LenDist ln d -> State.gets (repetition ln d) >>= \ws ->
		State.modify (`appendR` ws) >> Pipe.yield (Right $ BS.pack ws)
	EndOfInput -> pure ()

runLength' :: (
	Union.Member Pipe.P effs,
	Union.Member (State.S (Seq.Seq Word8)) effs ) =>
	Eff.E effs R (Either Word8 BS.ByteString) ()
runLength' = (Pipe.isMore >>=) . bool (pure ())
	$ Pipe.await >>= \rl -> (>> runLength) $ ($ rl) \case
		Literal w -> State.modify (`snoc` w) >> Pipe.yield (Left w)
		LiteralBS bs ->
			State.modify (`appendR` BS.unpack bs) >> Pipe.yield (Right bs)
		LenDist ln d -> State.gets (repetition ln d) >>= \ws ->
			State.modify (`appendR` ws) >> Pipe.yield (Right $ BS.pack ws)
		EndOfInput -> pure ()

data R
	= Literal Word8 | LiteralBS BS.ByteString | LenDist Length Dist
	| EndOfInput
	deriving Show

type Length = Int

type Dist = Int

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

runLengthToLitLen :: R -> [Int]
runLengthToLitLen (Literal b) = [fromIntegral b]
runLengthToLitLen (LiteralBS bs) = fromIntegral <$> BS.unpack bs
runLengthToLitLen (LenDist ln _dst) = [fst $ lengthToCode ln]
runLengthToLitLen EndOfInput = [256]

toLitLenFreqs :: [R] -> [(Int, Int)]
toLitLenFreqs = ((head &&& length) <$>) . L.group . L.sort . (runLengthToLitLen =<<)

runLengthToDist :: R -> [Int]
runLengthToDist (Literal _) = []
runLengthToDist (LiteralBS _) = []
runLengthToDist (LenDist _ln dst) = [fst $ distToCode dst]
runLengthToDist EndOfInput = []

toDistFreqs :: [R] -> [(Int, Int)]
toDistFreqs = ((head &&& length) <$>) . L.group . L.sort . (runLengthToDist =<<)
