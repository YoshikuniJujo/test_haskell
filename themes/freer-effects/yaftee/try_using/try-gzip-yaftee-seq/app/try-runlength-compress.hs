{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds, ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Arrow
import Control.Monad
import Control.Monad.Fix
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Pipe.Tools qualified as PipeT
import Control.Monad.Yaftee.Pipe.List qualified as PipeL
import Control.Monad.Yaftee.Pipe.Sequence.Crc32 qualified as PipeCrc32
import Control.Monad.Yaftee.Pipe.ByteString qualified as PipeBS
import Control.Monad.Yaftee.State qualified as State
import Control.HigherOpenUnion qualified as U
import Data.TypeLevel.List
import Data.Foldable
import Data.HigherFunctor qualified as HFunctor
import Data.Bits
import Data.Sequence qualified as Seq
import Data.Sequence.Word8 qualified as Seq
import Data.Bool
import Data.Word
import Data.ByteString qualified as BS
import System.IO
import System.Environment

import Pipe.Runlength.Compress qualified as Runlength
import Data.Gzip.Header
import Data.Deflate.Block
import Data.Word.Crc32 qualified as Crc32

main :: IO ()
main = do
	fp : fpo : _ <- getArgs
	h <- openFile fp ReadMode
	ho <- openFile fpo WriteMode

	_ <- Eff.runM . gzipRun_ @"foobar" . Pipe.run
		$ PipeBS.hGet 32 h Pipe.=$=
			PipeT.convert' bsToSeq Pipe.=$=
			gzipCompress "foobar" sampleGzipHeader Pipe.=$=
			PipeT.convert seqToBs Pipe.=$= PipeBS.hPutStr ho

	hClose h
	hClose ho

gzipRun_ :: forall nm es i o r . HFunctor.Loose (U.U es) =>
	Eff.E (States nm `Append` es) i o r -> Eff.E es i o ()
gzipRun_ = void
	. PipeT.lengthRun @nm
	. flip (State.runN @nm) Crc32.initial
	. flip (State.runN @nm) (([], []) :: Queue)
	. Runlength.run_ @nm

type States nm = Runlength.States nm `Append` '[
	State.Named nm Queue,
	State.Named nm Crc32.C,
	State.Named nm PipeT.Length ]

gzipCompress :: forall nm -> (
	U.Member Pipe.P es,
	GzipMembers nm es ) =>
	GzipHeader -> Eff.E es (Seq.Seq Word8) (Seq.Seq Word8) ()
gzipCompress nm hdr = do
	Pipe.yield $ encodeGzipHeader hdr
	_ <- PipeCrc32.crc32' nm Pipe.=$=
		PipeT.length nm Pipe.=$=
		Runlength.compress nm Pipe.=$=
		PipeL.bundle' 15 Pipe.=$=
		PipeT.convert'' runLengthsToBits [] Pipe.=$=
		toSequence' nm
	PipeCrc32.complement nm
	Pipe.yield . Seq.fromBits' =<< State.getN @Crc32.C nm
	Pipe.yield . Seq.fromBits' @Word32 . fromIntegral =<< State.getN @PipeT.Length nm

type GzipMembers nm es = (
	Runlength.Members nm es,
	U.Member (State.Named nm PipeT.Length) es,
	U.Member (State.Named nm Crc32.C) es,
	U.Member (State.Named nm Queue) es )

bsToSeq :: BS.ByteString -> Seq.Seq Word8
bsToSeq = Seq.fromList . BS.unpack

seqToBs :: Seq.Seq Word8 -> BS.ByteString
seqToBs = BS.pack . toList

type Bit = Bool

pattern O :: Bit
pattern O = False

type Queue = ([Bit], [Bit])

append :: Queue -> [Bit] -> Queue
append (xs, ys) bs = (xs, reverse bs ++ ys)

uncons :: Queue -> Maybe (Bit, Queue)
uncons = \case
	([], []) -> Nothing
	([], ys) -> uncons (reverse ys, [])
	(x : xs, ys) -> Just (x, (xs, ys))

popByte :: Queue -> Maybe (Word8, Queue)
popByte = ((listToNum `first`) <$>) . unfoldrN 8 uncons

listToNum :: Bits n => [Bit] -> n
listToNum = foldr (\b s -> bool id (`setBit` 0) b $ s `shiftL` 1) zeroBits

toSequence' :: forall nm -> (U.Member Pipe.P es, U.Member (State.Named nm Queue) es) =>
	Eff.E es [Bit] (Seq.Seq Word8) ()
toSequence' nm = fix \go -> Pipe.isMore >>= bool
	do	State.modifyN nm $ (`append` [O, O, O, O, O, O, O])
		Pipe.yield =<< uncurry (<$)
			. (Seq.fromList *** State.putN nm)
			. unfoldr' popByte =<< State.getN nm 
	do	State.modifyN nm . flip append =<< Pipe.await
		Pipe.yield =<< uncurry (<$)
			. (Seq.fromList *** State.putN nm)
			. unfoldr' popByte =<< State.getN nm
		go

unfoldr' :: (b -> Maybe (a, b)) -> b -> ([a], b)
unfoldr' f = fix \go s -> maybe ([], s) (\(x, s') -> (x :) `first` go s') $ f s

unfoldrN :: Int -> (a -> Maybe (b, a)) -> a -> Maybe ([b], a)
unfoldrN 0 _ x = Just ([], x)
unfoldrN n f x = case f x of
	Nothing -> Nothing
	Just (y, x') -> ((y :) `first`) <$> unfoldrN (n - 1) f x'
