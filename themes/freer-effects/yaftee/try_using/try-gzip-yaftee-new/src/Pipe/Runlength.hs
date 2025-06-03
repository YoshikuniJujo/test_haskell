{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Pipe.Runlength where

import Control.Arrow
import Data.List qualified as L
import Data.Word
import Data.ByteString.Lazy qualified as LBS
import Data.Gzip.Calc

import Control.Monad
import Control.Monad.Fix
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.State qualified as State
import Control.HigherOpenUnion qualified as U
import Data.TypeLevel.List (Append)
import Data.Foldable
import Data.HigherFunctor qualified as HFunctor
import Data.Sequence qualified as Seq

run_ :: HFunctor.Loose (U.U es) =>
	Eff.E (States nm `Append` es) i o a -> Eff.E es i o ()
run_ = void . flip State.runN (Seq Seq.empty)

type States nm = '[State.Named nm Seq]

runlength :: forall nm -> (U.Member Pipe.P es, Members nm es) =>
	Eff.E es R (Either Word8 LBS.ByteString) ()
runlength nm = fix \go -> Pipe.await >>= \rl -> ($ rl) \case
	Literal w -> (>> go)
		$ State.modifyN nm (`snoc` w) >> Pipe.yield (Left w)
	LiteralBS bs -> (>> go)
		$ State.modifyN nm (`appendR` LBS.unpack bs) >> Pipe.yield (Right bs)
	LenDist ln d -> (>> go) $ State.getsN nm (repetition ln d) >>= \ws ->
		State.modifyN nm (`appendR` ws) >> Pipe.yield (Right $ LBS.pack ws)
	EndOfInput -> pure ()

type Members nm es = (U.Member (State.Named nm Seq) es)

newtype Seq = Seq { unSeq :: Seq.Seq Word8 }

repetition :: Int -> Int -> Seq -> [Word8]
repetition r d (Seq ws) = takeRep r ws' ws'
	where ws' = toList . Seq.take r $ takeR d ws

takeRep :: Int -> [a] -> [a] -> [a]
takeRep 0 _ _ = []
takeRep n xs0 (x : xs) = x : takeRep (n - 1) xs0 xs
takeRep n xs0 [] = takeRep n xs0 xs0

takeR :: Int -> Seq.Seq Word8 -> Seq.Seq Word8
takeR n xs = Seq.drop (Seq.length xs - n) xs

snoc :: Seq -> Word8 -> Seq
snoc (Seq s) w = let s' = if ln > 32768 then Seq.drop (ln - 32768) s else s in
	Seq $ s' Seq.|> w
	where ln = Seq.length s

appendR :: Seq -> [Word8] -> Seq
appendR (Seq s) ws = let s' = if ln > 32768 then Seq.drop (ln - 32768) s else s in
	Seq $ foldl (Seq.|>) s' ws
	where ln = Seq.length s

data R = Literal Word8 | LiteralBS LBS.ByteString
	| LenDist Length Dist | EndOfInput deriving Show

toByteString :: R -> LBS.ByteString
toByteString (LiteralBS bs) = bs
toByteString (Literal w) = LBS.pack [w]

type Length = Int
type Dist = Int

runLengthToLitLen :: R -> [Int]
runLengthToLitLen (Literal b) = [fromIntegral b]
runLengthToLitLen (LiteralBS bs) = fromIntegral <$> LBS.unpack bs
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
toDistFreqs = addDummyDistFreqs . ((head &&& length) <$>) . L.group . L.sort . (runLengthToDist =<<)

addDummyDistFreqs :: [(Int, Int)] -> [(Int, Int)]
addDummyDistFreqs fs = fs ++ take (2 - length fs) [(28, 1), (29, 1)]
