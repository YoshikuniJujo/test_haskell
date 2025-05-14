{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Pipe.RunLength where

import Control.Arrow
import Data.List qualified as L
import Data.Word
import Data.ByteString qualified as BS
import Data.Gzip.Calc

import Control.Monad.Fix
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.State qualified as State
import Control.HigherOpenUnion qualified as U
import Data.Foldable
import Data.Sequence qualified as Seq

runLength :: (
	U.Member Pipe.P es,
	U.Member (State.S (Seq.Seq Word8)) es ) =>
	Eff.E es R (Either Word8 BS.ByteString) ()
runLength = fix \go -> Pipe.await >>= \rl -> ($ rl) \case
	Literal w -> (>> go)
		$ State.modify (`snoc` w) >> Pipe.yield (Left w)
	LiteralBS bs -> (>> go)
		$ State.modify (`appendR` BS.unpack bs) >> Pipe.yield (Right bs)
	LenDist ln d -> (>> go) $ State.gets (repetition ln d) >>= \ws ->
		State.modify (`appendR` ws) >> Pipe.yield (Right $ BS.pack ws)
	EndOfInput -> pure ()

repetition :: Int -> Int -> Seq.Seq Word8 -> [Word8]
repetition r d ws = takeRep r ws' ws'
	where ws' = toList . Seq.take r $ takeR d ws

takeRep :: Int -> [a] -> [a] -> [a]
takeRep 0 _ _ = []
takeRep n xs0 (x : xs) = x : takeRep (n - 1) xs0 xs
takeRep n xs0 [] = takeRep n xs0 xs0

takeR :: Int -> Seq.Seq Word8 -> Seq.Seq Word8
takeR n xs = Seq.drop (Seq.length xs - n) xs

snoc :: Seq.Seq Word8 -> Word8 -> Seq.Seq Word8
snoc s w = let s' = if ln > 32768 then Seq.drop (ln - 32768) s else s in
	s' Seq.|> w
	where ln = Seq.length s

appendR :: Seq.Seq Word8 -> [Word8] -> Seq.Seq Word8
appendR s ws = let s' = if ln > 32768 then Seq.drop (ln - 32768) s else s in
	foldl (Seq.|>) s' ws
	where ln = Seq.length s

data R = Literal Word8 | LiteralBS BS.ByteString
	| LenDist Length Dist | EndOfInput deriving Show

type Length = Int
type Dist = Int

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
