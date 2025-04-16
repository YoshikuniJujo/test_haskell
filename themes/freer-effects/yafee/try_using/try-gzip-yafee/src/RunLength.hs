{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module RunLength (

	runLength, RunLength(..), RunLengthLength, RunLengthDist,

	runLengthToWord32

	) where

import Control.Monad.Yafee.Eff qualified as Eff
import Control.Monad.Yafee.Pipe qualified as Pipe
import Control.Monad.Yafee.State qualified as State
import Control.OpenUnion qualified as Union
import Data.Foldable
import Data.Word
import Data.Sequence qualified as Seq
import Data.ByteString qualified as BS

import Calc

runLength :: (Union.Member (State.S (Seq.Seq Word8)) effs) =>
	Eff.E (Pipe.P RunLength (Either Word8 BS.ByteString) ': effs) ()
runLength = Pipe.await >>= maybe (pure ()) \rl -> (>> runLength) $ ($ rl) \case
	RunLengthLiteral w -> State.modify (`snoc` w) >> Pipe.yield (Left w)
	RunLengthLiteralBS bs ->
		State.modify (`appendR` BS.unpack bs) >> Pipe.yield (Right bs)
	RunLengthLenDist ln d -> State.gets (repetition ln d) >>= \ws ->
		State.modify (`appendR` ws) >> Pipe.yield (Right $ BS.pack ws)

data RunLength =
	RunLengthLiteralBS BS.ByteString |
	RunLengthLiteral Word8 | RunLengthLenDist RunLengthLength RunLengthDist
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
runLengthToWord32 (RunLengthLiteralBS bs) = fromLiteral' . fromIntegral <$> BS.unpack bs
runLengthToWord32 (RunLengthLiteral b) = [fromLiteral' $ fromIntegral b]
runLengthToWord32 (RunLengthLenDist ln dst) = [fromLength' ln, fromDist' dst]
